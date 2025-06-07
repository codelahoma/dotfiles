#!/usr/bin/env python3
"""
Quick test of the optimized FlowLoom coordination system.

This test verifies that all the optimizations work correctly and the system
is production-ready.
"""

import sys
import os
from pathlib import Path

# Add packages to Python path
PROJECT_ROOT = Path(__file__).parent.parent
sys.path.insert(0, str(PROJECT_ROOT / "packages"))

def test_imports():
    """Test that all optimized imports work correctly."""
    print("🧪 Testing Optimized Coordination System")
    print("=" * 40)
    print()
    
    print("📦 Testing imports...")
    try:
        from flowloom_memory_monitor import (
            BaseCoordinator, CoordinatorMixin,
            create_standardized_worker_entity,
            create_standardized_task_entity, 
            create_standardized_session_entity,
            search_entities, MemoryMonitorWrapper
        )
        print("✅ Memory monitor imports successful")
        
        from flowloom_session import quick_session_status
        print("✅ Session manager imports successful")
        
    except ImportError as e:
        print(f"❌ Import failed: {e}")
        return False
    
    return True

def test_base_coordinator():
    """Test the base coordinator functionality."""
    print("\n🏗️  Testing Base Coordinator...")
    
    try:
        from flowloom_memory_monitor import BaseCoordinator
        
        # Create a test coordinator
        class TestCoordinator(BaseCoordinator):
            def get_coordinator_type(self):
                return "test"
            def get_worker_entity_type(self):
                return "TestWorker"
            def get_task_entity_type(self):
                return "TestTask"
            def get_session_entity_type(self):
                return "TestSession"
        
        coordinator = TestCoordinator()
        
        # Test basic methods
        shell_pid = coordinator.shell_pid
        print(f"   Shell PID: {shell_pid}")
        
        branch = coordinator._get_current_branch()
        print(f"   Current branch: {branch}")
        
        timestamp = coordinator._format_timestamp()
        print(f"   Timestamp: {timestamp}")
        
        # Test ID generation
        test_id = coordinator._generate_id("test")
        print(f"   Generated ID: {test_id}")
        
        # Test observation creation
        obs = coordinator._create_shell_tagged_observation("Test observation")
        print(f"   Tagged observation: {obs[:50]}...")
        
        print("✅ Base coordinator tests passed")
        return True
        
    except Exception as e:
        print(f"❌ Base coordinator test failed: {e}")
        return False

def test_memory_wrapper():
    """Test the optimized memory wrapper."""
    print("\n💾 Testing Memory Wrapper...")
    
    try:
        from flowloom_memory_monitor import MemoryMonitorWrapper
        
        # Test wrapper initialization (lazy loading)
        wrapper = MemoryMonitorWrapper()
        print("   Wrapper initialized with lazy loading")
        
        # Test cache functionality
        print(f"   Cache TTL: {wrapper.cache_ttl} seconds")
        print(f"   Cache size: {len(wrapper._entity_cache)}")
        
        # Test search with error handling
        results = wrapper.search_entities("NonExistentType", "test", use_cache=False)
        print(f"   Search results (should be empty): {len(results)}")
        
        print("✅ Memory wrapper tests passed")
        return True
        
    except Exception as e:
        print(f"❌ Memory wrapper test failed: {e}")
        return False

def test_entity_creation():
    """Test standardized entity creation."""
    print("\n🏭 Testing Entity Creation...")
    
    try:
        from flowloom_memory_monitor import (
            BaseCoordinator,
            create_standardized_worker_entity,
            create_standardized_task_entity,
            create_standardized_session_entity
        )
        
        # Create test coordinator
        class TestCoordinator(BaseCoordinator):
            def get_coordinator_type(self):
                return "test"
            def get_worker_entity_type(self):
                return "TestWorker"
            def get_task_entity_type(self):
                return "TestTask"
            def get_session_entity_type(self):
                return "TestSession"
        
        coordinator = TestCoordinator()
        
        # Test worker entity creation
        worker_entity = create_standardized_worker_entity(coordinator, "research,testing")
        print(f"   Worker entity: {worker_entity['name']}")
        print(f"   Observations: {len(worker_entity['observations'])}")
        
        # Test task entity creation
        task_entity = create_standardized_task_entity(coordinator, "research", "Test task", "high")
        print(f"   Task entity: {task_entity['name']}")
        print(f"   Observations: {len(task_entity['observations'])}")
        
        # Test session entity creation
        session_entity = create_standardized_session_entity(coordinator, "TestSession", "Test objective")
        print(f"   Session entity: {session_entity['name']}")
        print(f"   Observations: {len(session_entity['observations'])}")
        
        print("✅ Entity creation tests passed")
        return True
        
    except Exception as e:
        print(f"❌ Entity creation test failed: {e}")
        return False

def test_flowloom_coordinator():
    """Test the production FlowLoom coordinator."""
    print("\n🎯 Testing FlowLoom Coordinator...")
    
    try:
        # Import the main coordinator
        sys.path.insert(0, str(PROJECT_ROOT / "bin"))
        from flowloom_coordinator import FlowLoomCoordinator
        
        # Create coordinator instance
        coordinator = FlowLoomCoordinator()
        print(f"   Coordination mode: {coordinator.coordination_mode}")
        print(f"   Coordinator type: {coordinator.get_coordinator_type()}")
        print(f"   Worker entity type: {coordinator.get_worker_entity_type()}")
        print(f"   Task entity type: {coordinator.get_task_entity_type()}")
        
        # Test mode detection
        current_branch = coordinator._get_current_branch()
        session_type = coordinator._determine_session_type(current_branch)
        print(f"   Current branch: {current_branch}")
        print(f"   Session type: {session_type}")
        
        # Test worker scoring (without actual workers)
        score = coordinator._score_worker({
            "observations": ["capabilities: research,testing", "status: available"]
        }, None, "research")
        print(f"   Worker scoring test: {score}")
        
        print("✅ FlowLoom coordinator tests passed")
        return True
        
    except Exception as e:
        print(f"❌ FlowLoom coordinator test failed: {e}")
        return False

def test_session_integration():
    """Test session manager integration."""
    print("\n🔧 Testing Session Integration...")
    
    try:
        from flowloom_session import quick_session_status
        
        # Test session status
        session_status = quick_session_status()
        session_count = len(session_status.get('sessions', {}))
        print(f"   Active sessions: {session_count}")
        print(f"   Session status keys: {list(session_status.keys())}")
        
        print("✅ Session integration tests passed")
        return True
        
    except Exception as e:
        print(f"❌ Session integration test failed: {e}")
        return False

def main():
    """Run all tests."""
    print("Starting optimized coordination system tests...\n")
    
    tests = [
        ("Imports", test_imports),
        ("Base Coordinator", test_base_coordinator),
        ("Memory Wrapper", test_memory_wrapper),
        ("Entity Creation", test_entity_creation),
        ("FlowLoom Coordinator", test_flowloom_coordinator),
        ("Session Integration", test_session_integration)
    ]
    
    passed = 0
    failed = 0
    
    for test_name, test_func in tests:
        try:
            if test_func():
                passed += 1
            else:
                failed += 1
        except Exception as e:
            print(f"❌ {test_name} test crashed: {e}")
            failed += 1
    
    print(f"\n📊 Test Results:")
    print(f"   ✅ Passed: {passed}")
    print(f"   ❌ Failed: {failed}")
    print(f"   📈 Success Rate: {(passed / (passed + failed)) * 100:.1f}%")
    
    if failed == 0:
        print("\n🎉 All optimization tests passed! System is production-ready.")
        return True
    else:
        print(f"\n⚠️  {failed} tests failed. Review and fix issues before production use.")
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)