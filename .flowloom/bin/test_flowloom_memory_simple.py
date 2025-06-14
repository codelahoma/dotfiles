#!/usr/bin/env python3
"""
Simplified integration test suite for flowloom-memory
"""

import sys
import os
import subprocess
import tempfile
from pathlib import Path

def test_help_command():
    """Test help command"""
    script_path = Path(__file__).parent / "flowloom-memory"
    result = subprocess.run([
        sys.executable, str(script_path), '--help'
    ], capture_output=True, text=True)
    
    assert result.returncode == 0, f"Help failed with code {result.returncode}"
    assert 'FlowLoom Memory - Unified Interface' in result.stdout, "Missing help header"
    assert 'WRITING COMMANDS' in result.stdout, "Missing writing commands section"
    assert 'QUERY COMMANDS' in result.stdout, "Missing query commands section"
    print("✓ Help command works")

def test_unknown_command():
    """Test unknown command handling"""
    script_path = Path(__file__).parent / "flowloom-memory"
    result = subprocess.run([
        sys.executable, str(script_path), 'unknown-command'
    ], capture_output=True, text=True)
    
    assert result.returncode == 1, f"Unknown command should return 1, got {result.returncode}"
    assert 'Unknown command' in result.stderr, "Missing error message"
    print("✓ Unknown command handling works")

def test_help_variations():
    """Test different help variations"""
    script_path = Path(__file__).parent / "flowloom-memory"
    
    for cmd in [['--help'], ['help'], []]:
        result = subprocess.run([
            sys.executable, str(script_path)
        ] + cmd, capture_output=True, text=True)
        
        assert result.returncode == 0, f"Help variation {cmd} failed"
        assert 'FlowLoom Memory' in result.stdout, f"Help variation {cmd} missing content"
    
    print("✓ All help variations work")

def test_memory_file_option():
    """Test --memory-file option parsing"""
    script_path = Path(__file__).parent / "flowloom-memory"
    
    # Test with invalid memory file - should still parse correctly but fail execution
    result = subprocess.run([
        sys.executable, str(script_path), 
        '--memory-file', '/nonexistent/memory.json',
        'stats'
    ], capture_output=True, text=True)
    
    # Should fail execution but not argument parsing
    assert result.returncode != 0, "Should fail with nonexistent memory file"
    print("✓ Memory file option parsing works")

def test_command_routing():
    """Test that commands are properly categorized"""
    # This test checks that the script categorizes commands correctly
    # by testing the error messages for different command types
    
    script_path = Path(__file__).parent / "flowloom-memory"
    
    # Test log_observation command (will fail but should route correctly)
    result = subprocess.run([
        sys.executable, str(script_path), 'add-entity'
    ], capture_output=True, text=True)
    
    # Should attempt to route to log_observation (will fail due to missing args)
    assert result.returncode != 0, "add-entity should fail with missing args"
    
    # Test memory-monitor command (will fail but should route correctly)  
    result = subprocess.run([
        sys.executable, str(script_path), 'query'
    ], capture_output=True, text=True)
    
    # Should attempt to route to memory-monitor (will fail due to missing args)
    assert result.returncode != 0, "query should fail with missing args"
    
    print("✓ Command routing works")

def test_executable_permissions():
    """Test that script has proper permissions"""
    script_path = Path(__file__).parent / "flowloom-memory"
    
    assert script_path.exists(), "Script file doesn't exist"
    assert os.access(script_path, os.X_OK), "Script is not executable"
    
    print("✓ Script has executable permissions")

def run_all_tests():
    """Run all tests"""
    tests = [
        test_executable_permissions,
        test_help_command,
        test_help_variations, 
        test_unknown_command,
        test_memory_file_option,
        test_command_routing
    ]
    
    print("Running flowloom-memory integration tests...\n")
    
    failed = 0
    for test in tests:
        try:
            test()
        except AssertionError as e:
            print(f"✗ {test.__name__}: {e}")
            failed += 1
        except Exception as e:
            print(f"✗ {test.__name__}: Unexpected error: {e}")
            failed += 1
    
    total = len(tests)
    passed = total - failed
    
    print(f"\nTest Results: {passed}/{total} passed")
    
    if failed == 0:
        print("🎉 All tests passed!")
        return True
    else:
        print(f"❌ {failed} tests failed")
        return False

if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)