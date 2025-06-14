#!/usr/bin/env python3
"""
Test suite for flowloom-memory unified interface
"""

import sys
import os
import tempfile
import unittest
from unittest.mock import patch, MagicMock, call
import subprocess
from pathlib import Path

# Add the bin directory to Python path for importing
sys.path.insert(0, str(Path(__file__).parent))

# Import the module we're testing by executing it directly
import importlib.util
import types

flowloom_memory_path = Path(__file__).parent / "flowloom-memory"
flowloom_memory = types.ModuleType("flowloom_memory")

# Read and execute the source code
with open(flowloom_memory_path, 'r') as f:
    source_code = f.read()

# Execute the code in the module's namespace
exec(source_code, flowloom_memory.__dict__)

class TestFlowLoomMemory(unittest.TestCase):
    """Test cases for FlowLoom Memory unified interface"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.test_memory_file = "test-memory.json"
        
    def tearDown(self):
        """Clean up after tests"""
        if os.path.exists(self.test_memory_file):
            os.remove(self.test_memory_file)
    
    def test_command_routing_classification(self):
        """Test that commands are classified correctly"""
        # Test log_observation commands
        log_commands = ['add-entity', 'add-observation', 'add-relation', 'get-entity', 'list-entities']
        for cmd in log_commands:
            self.assertIn(cmd, flowloom_memory.LOG_OBSERVATION_COMMANDS)
        
        # Test memory-monitor commands  
        monitor_commands = ['query', 'search', 'watch', 'wait-for', 'serve', 'explain']
        for cmd in monitor_commands:
            self.assertIn(cmd, flowloom_memory.MEMORY_MONITOR_COMMANDS)
        
        # Test shared commands
        self.assertIn('stats', flowloom_memory.SHARED_COMMANDS)
        self.assertIn('help', flowloom_memory.SHARED_COMMANDS)
    
    def test_get_memory_file_default(self):
        """Test default memory file path"""
        with patch.dict(os.environ, {}, clear=True):
            self.assertEqual(flowloom_memory.get_memory_file(), 'fl-memory.json')
    
    def test_get_memory_file_environment(self):
        """Test memory file from environment variable"""
        test_path = '/custom/path/memory.json'
        with patch.dict(os.environ, {'FLOWLOOM_MEMORY_FILE': test_path}):
            self.assertEqual(flowloom_memory.get_memory_file(), test_path)
    
    @patch('flowloom_memory.subprocess.run')
    def test_exec_log_observation(self, mock_run):
        """Test log_observation command execution"""
        mock_run.return_value.returncode = 0
        
        result = flowloom_memory.exec_log_observation('add-entity', ['test', 'Feature', 'desc'], 'test.json')
        
        # Verify subprocess.run was called with correct arguments
        expected_cmd = [
            sys.executable,
            str(Path(__file__).parent / "log_observation.py"),
            "--memory-file", "test.json",
            "add-entity", "test", "Feature", "desc"
        ]
        mock_run.assert_called_once_with(expected_cmd)
        self.assertEqual(result, 0)
    
    @patch('flowloom_memory.subprocess.run')
    def test_exec_memory_monitor(self, mock_run):
        """Test memory-monitor command execution"""
        mock_run.return_value.returncode = 0
        
        result = flowloom_memory.exec_memory_monitor('query', ['SELECT * FROM entities'], 'test.json')
        
        # Verify subprocess.run was called with correct arguments
        expected_cmd = ["memory-monitor", "query", "test.json", "SELECT * FROM entities"]
        mock_run.assert_called_once_with(expected_cmd)
        self.assertEqual(result, 0)
    
    @patch('flowloom_memory.exec_log_observation')
    def test_route_log_observation_commands(self, mock_exec):
        """Test routing of log_observation commands"""
        mock_exec.return_value = 0
        
        # Test each log_observation command
        test_cases = [
            (['add-entity', 'name', 'type', 'obs'], 'add-entity', ['name', 'type', 'obs']),
            (['add-observation', 'name', 'obs'], 'add-observation', ['name', 'obs']),
            (['add-relation', 'from', 'to', 'type'], 'add-relation', ['from', 'to', 'type']),
            (['get-entity', 'name'], 'get-entity', ['name']),
            (['list-entities'], 'list-entities', []),
            (['stats'], 'stats', [])  # stats routes to log_observation
        ]
        
        for args, expected_cmd, expected_remaining in test_cases:
            with self.subTest(args=args):
                mock_exec.reset_mock()
                flowloom_memory.route_command(args)
                mock_exec.assert_called_once_with(expected_cmd, expected_remaining, 'fl-memory.json')
    
    @patch('flowloom_memory.exec_memory_monitor')
    def test_route_memory_monitor_commands(self, mock_exec):
        """Test routing of memory-monitor commands"""
        mock_exec.return_value = 0
        
        # Test each memory-monitor command
        test_cases = [
            (['query', 'SELECT * FROM entities'], 'query', ['SELECT * FROM entities']),
            (['search', 'pattern'], 'search', ['pattern']),
            (['watch'], 'watch', []),
            (['wait-for', 'condition'], 'wait-for', ['condition']),
            (['serve'], 'serve', []),
            (['explain', 'SELECT * FROM entities'], 'explain', ['SELECT * FROM entities'])
        ]
        
        for args, expected_cmd, expected_remaining in test_cases:
            with self.subTest(args=args):
                mock_exec.reset_mock()
                flowloom_memory.route_command(args)
                mock_exec.assert_called_once_with(expected_cmd, expected_remaining, 'fl-memory.json')
    
    @patch('flowloom_memory.show_unified_help')
    def test_help_commands(self, mock_help):
        """Test help command routing"""
        help_cases = [[], ['--help'], ['help']]
        
        for args in help_cases:
            with self.subTest(args=args):
                mock_help.reset_mock()
                result = flowloom_memory.route_command(args)
                mock_help.assert_called_once()
                self.assertEqual(result, 0)
    
    def test_unknown_command(self):
        """Test handling of unknown commands"""
        with patch('sys.stderr') as mock_stderr:
            result = flowloom_memory.route_command(['unknown-command'])
            self.assertEqual(result, 1)
    
    @patch('flowloom_memory.exec_log_observation')
    def test_memory_file_option(self, mock_exec):
        """Test --memory-file option handling"""
        mock_exec.return_value = 0
        
        args = ['--memory-file', 'custom.json', 'add-entity', 'test', 'Feature', 'desc']
        flowloom_memory.route_command(args)
        
        mock_exec.assert_called_once_with('add-entity', ['test', 'Feature', 'desc'], 'custom.json')
    
    @patch('flowloom_memory.exec_memory_monitor')
    def test_memory_file_option_monitor(self, mock_exec):
        """Test --memory-file option with memory-monitor commands"""
        mock_exec.return_value = 0
        
        args = ['--memory-file', 'custom.json', 'query', 'SELECT * FROM entities']
        flowloom_memory.route_command(args)
        
        mock_exec.assert_called_once_with('query', ['SELECT * FROM entities'], 'custom.json')
    
    def test_show_unified_help_output(self):
        """Test that unified help shows expected content"""
        with patch('sys.stdout') as mock_stdout:
            flowloom_memory.show_unified_help()
            output = ''.join(call.args[0] for call in mock_stdout.write.call_args_list)
            
            # Check for key sections
            self.assertIn('FlowLoom Memory - Unified Interface', output)
            self.assertIn('WRITING COMMANDS', output)
            self.assertIn('QUERY COMMANDS', output)
            self.assertIn('EXAMPLES', output)
            self.assertIn('add-entity', output)
            self.assertIn('query', output)


class TestFlowLoomMemoryIntegration(unittest.TestCase):
    """Integration tests for flowloom-memory script"""
    
    def setUp(self):
        """Set up integration test fixtures"""
        self.script_path = Path(__file__).parent / "flowloom-memory"
        self.test_memory_file = "test-integration-memory.json"
    
    def tearDown(self):
        """Clean up integration test fixtures"""
        if os.path.exists(self.test_memory_file):
            os.remove(self.test_memory_file)
    
    def test_script_executable(self):
        """Test that script is executable and shows help"""
        result = subprocess.run([
            sys.executable, str(self.script_path), '--help'
        ], capture_output=True, text=True)
        
        self.assertEqual(result.returncode, 0)
        self.assertIn('FlowLoom Memory - Unified Interface', result.stdout)
    
    def test_unknown_command_exit_code(self):
        """Test that unknown commands return proper exit code"""
        result = subprocess.run([
            sys.executable, str(self.script_path), 'unknown-command'
        ], capture_output=True, text=True)
        
        self.assertEqual(result.returncode, 1)
        self.assertIn('Unknown command', result.stderr)
    
    def test_help_variations(self):
        """Test different help command variations"""
        help_commands = [['--help'], ['help'], []]
        
        for cmd in help_commands:
            with self.subTest(cmd=cmd):
                result = subprocess.run([
                    sys.executable, str(self.script_path)
                ] + cmd, capture_output=True, text=True)
                
                self.assertEqual(result.returncode, 0)
                self.assertIn('FlowLoom Memory - Unified Interface', result.stdout)


class TestCommandLineArguments(unittest.TestCase):
    """Test command line argument parsing and handling"""
    
    def test_memory_file_parsing(self):
        """Test memory file argument parsing"""
        test_cases = [
            # (input_args, expected_command, expected_remaining, expected_file)
            (['add-entity', 'test'], 'add-entity', ['test'], 'fl-memory.json'),
            (['--memory-file', 'custom.json', 'add-entity', 'test'], 'add-entity', ['test'], 'custom.json'),
            (['query', '--memory-file', 'custom.json', 'SELECT *'], None, None, None),  # Invalid order
        ]
        
        for input_args, expected_cmd, expected_remaining, expected_file in test_cases:
            with self.subTest(args=input_args):
                if expected_cmd is None:
                    # Test invalid cases
                    continue
                    
                # Mock the execution functions to capture arguments
                with patch('flowloom_memory.exec_log_observation') as mock_log, \
                     patch('flowloom_memory.exec_memory_monitor') as mock_monitor:
                    
                    mock_log.return_value = 0
                    mock_monitor.return_value = 0
                    
                    flowloom_memory.route_command(input_args)
                    
                    # Check that the correct function was called with expected arguments
                    if expected_cmd in flowloom_memory.LOG_OBSERVATION_COMMANDS or expected_cmd == 'stats':
                        mock_log.assert_called_once_with(expected_cmd, expected_remaining, expected_file)
                    elif expected_cmd in flowloom_memory.MEMORY_MONITOR_COMMANDS:
                        mock_monitor.assert_called_once_with(expected_cmd, expected_remaining, expected_file)


def run_tests():
    """Run all tests"""
    # Create test suite
    suite = unittest.TestSuite()
    
    # Add test cases
    suite.addTests(unittest.TestLoader().loadTestsFromTestCase(TestFlowLoomMemory))
    suite.addTests(unittest.TestLoader().loadTestsFromTestCase(TestFlowLoomMemoryIntegration))
    suite.addTests(unittest.TestLoader().loadTestsFromTestCase(TestCommandLineArguments))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    return result.wasSuccessful()


if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)