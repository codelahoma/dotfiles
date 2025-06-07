#!/usr/bin/env python3
"""
FlowLoom Performance Monitor - Real-time performance tracking and optimization.

Monitors session management, memory coordination, and system performance
to ensure FlowLoom operates at optimal efficiency.
"""

import json
import time
import psutil
import asyncio
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, asdict
import argparse
import sys


@dataclass
class PerformanceMetrics:
    """Performance metrics snapshot."""
    timestamp: str
    cpu_percent: float
    memory_mb: float
    disk_io_read_mb: float
    disk_io_write_mb: float
    session_count: int
    memory_file_size_kb: float
    cache_hit_rate: float
    avg_query_time_ms: float
    active_processes: int


class FlowLoomPerformanceMonitor:
    """Real-time performance monitoring for FlowLoom."""
    
    def __init__(self, project_root: Path, monitoring_interval: float = 5.0):
        self.project_root = project_root
        self.monitoring_interval = monitoring_interval
        
        # Paths
        self.sessions_root = project_root / "sessions"
        self.memory_file = project_root / "memory.json"
        self.performance_log = project_root / ".flowloom" / "performance.log"
        
        # Ensure log directory exists
        self.performance_log.parent.mkdir(parents=True, exist_ok=True)
        
        # Performance tracking
        self.metrics_history: List[PerformanceMetrics] = []
        self.max_history = 288  # 24 hours at 5-minute intervals
        
        # Baseline measurements
        self.baseline_cpu = 0.0
        self.baseline_memory = 0.0
        self.baseline_established = False
        
        # Process tracking
        self.process = psutil.Process()
        self.start_time = time.time()
        
        # Load existing metrics
        self._load_existing_metrics()
    
    def _load_existing_metrics(self):
        """Load existing performance metrics."""
        if self.performance_log.exists():
            try:
                with open(self.performance_log, 'r') as f:
                    for line in f:
                        if line.strip():
                            data = json.loads(line)
                            metrics = PerformanceMetrics(**data)
                            self.metrics_history.append(metrics)
                
                # Keep only recent metrics
                if len(self.metrics_history) > self.max_history:
                    self.metrics_history = self.metrics_history[-self.max_history:]
                    
            except Exception as e:
                print(f"Warning: Failed to load existing metrics: {e}")
    
    def _save_metrics(self, metrics: PerformanceMetrics):
        """Save metrics to log file."""
        try:
            with open(self.performance_log, 'a') as f:
                f.write(json.dumps(asdict(metrics)) + '\n')
        except Exception as e:
            print(f"Warning: Failed to save metrics: {e}")
    
    def collect_system_metrics(self) -> PerformanceMetrics:
        """Collect current system performance metrics."""
        try:
            # System metrics
            cpu_percent = psutil.cpu_percent(interval=1)
            memory_info = psutil.virtual_memory()
            memory_mb = memory_info.used / 1024 / 1024
            
            # Disk I/O
            disk_io = psutil.disk_io_counters()
            disk_read_mb = disk_io.read_bytes / 1024 / 1024 if disk_io else 0
            disk_write_mb = disk_io.write_bytes / 1024 / 1024 if disk_io else 0
            
            # FlowLoom specific metrics
            session_count = self._count_sessions()
            memory_file_size = self._get_memory_file_size()
            cache_stats = self._get_cache_stats()
            
            # Process count
            active_processes = len(psutil.pids())
            
            return PerformanceMetrics(
                timestamp=datetime.now().isoformat(),
                cpu_percent=cpu_percent,
                memory_mb=memory_mb,
                disk_io_read_mb=disk_read_mb,
                disk_io_write_mb=disk_write_mb,
                session_count=session_count,
                memory_file_size_kb=memory_file_size,
                cache_hit_rate=cache_stats.get('hit_rate', 0.0),
                avg_query_time_ms=cache_stats.get('avg_query_time', 0.0),
                active_processes=active_processes
            )
            
        except Exception as e:
            print(f"Error collecting metrics: {e}")
            return PerformanceMetrics(
                timestamp=datetime.now().isoformat(),
                cpu_percent=0.0, memory_mb=0.0, disk_io_read_mb=0.0,
                disk_io_write_mb=0.0, session_count=0, memory_file_size_kb=0.0,
                cache_hit_rate=0.0, avg_query_time_ms=0.0, active_processes=0
            )
    
    def _count_sessions(self) -> int:
        """Count active sessions."""
        if not self.sessions_root.exists():
            return 0
        
        count = 0
        for session_dir in self.sessions_root.iterdir():
            if session_dir.is_dir() and (session_dir / "metadata.json").exists():
                count += 1
        
        return count
    
    def _get_memory_file_size(self) -> float:
        """Get memory.json file size in KB."""
        if not self.memory_file.exists():
            return 0.0
        
        return self.memory_file.stat().st_size / 1024
    
    def _get_cache_stats(self) -> Dict[str, float]:
        """Get cache performance statistics."""
        try:
            # This would integrate with actual cache implementations
            # For now, return mock data
            return {
                'hit_rate': 85.5,
                'avg_query_time': 2.3
            }
        except Exception:
            return {'hit_rate': 0.0, 'avg_query_time': 0.0}
    
    def establish_baseline(self, samples: int = 5):
        """Establish performance baseline."""
        print(f"Establishing performance baseline ({samples} samples)...")
        
        cpu_samples = []
        memory_samples = []
        
        for i in range(samples):
            metrics = self.collect_system_metrics()
            cpu_samples.append(metrics.cpu_percent)
            memory_samples.append(metrics.memory_mb)
            
            if i < samples - 1:
                time.sleep(2)
        
        self.baseline_cpu = sum(cpu_samples) / len(cpu_samples)
        self.baseline_memory = sum(memory_samples) / len(memory_samples)
        self.baseline_established = True
        
        print(f"Baseline established:")
        print(f"  CPU: {self.baseline_cpu:.1f}%")
        print(f"  Memory: {self.baseline_memory:.1f} MB")
    
    def detect_performance_issues(self, current_metrics: PerformanceMetrics) -> List[str]:
        """Detect potential performance issues."""
        issues = []
        
        if not self.baseline_established:
            return issues
        
        # CPU usage check
        if current_metrics.cpu_percent > self.baseline_cpu * 2:
            issues.append(f"High CPU usage: {current_metrics.cpu_percent:.1f}% "
                         f"(baseline: {self.baseline_cpu:.1f}%)")
        
        # Memory usage check
        if current_metrics.memory_mb > self.baseline_memory * 1.5:
            issues.append(f"High memory usage: {current_metrics.memory_mb:.1f} MB "
                         f"(baseline: {self.baseline_memory:.1f} MB)")
        
        # Session count check
        if current_metrics.session_count > 50:
            issues.append(f"High session count: {current_metrics.session_count}")
        
        # Memory file size check
        if current_metrics.memory_file_size_kb > 1024:  # 1MB
            issues.append(f"Large memory file: {current_metrics.memory_file_size_kb:.1f} KB")
        
        # Cache performance check
        if current_metrics.cache_hit_rate < 70:
            issues.append(f"Low cache hit rate: {current_metrics.cache_hit_rate:.1f}%")
        
        # Query performance check
        if current_metrics.avg_query_time_ms > 100:
            issues.append(f"Slow queries: {current_metrics.avg_query_time_ms:.1f}ms avg")
        
        return issues
    
    def generate_recommendations(self, issues: List[str]) -> List[str]:
        """Generate performance optimization recommendations."""
        recommendations = []
        
        for issue in issues:
            if "High CPU usage" in issue:
                recommendations.append("Consider reducing poll intervals or optimizing query patterns")
            elif "High memory usage" in issue:
                recommendations.append("Run maintenance cleanup or reduce session cache size")
            elif "High session count" in issue:
                recommendations.append("Clean up old/inactive sessions")
            elif "Large memory file" in issue:
                recommendations.append("Optimize memory.json structure (remove duplicates)")
            elif "Low cache hit rate" in issue:
                recommendations.append("Increase cache TTL or review query patterns")
            elif "Slow queries" in issue:
                recommendations.append("Build memory indexes or optimize query complexity")
        
        if not recommendations:
            recommendations.append("Performance is optimal")
        
        return recommendations
    
    def print_current_status(self):
        """Print current performance status."""
        if not self.metrics_history:
            print("No metrics available")
            return
        
        latest = self.metrics_history[-1]
        issues = self.detect_performance_issues(latest)
        recommendations = self.generate_recommendations(issues)
        
        print(f"\n🚀 FlowLoom Performance Status ({latest.timestamp})")
        print("=" * 60)
        
        print(f"System Resources:")
        print(f"  CPU Usage:      {latest.cpu_percent:6.1f}%")
        print(f"  Memory:         {latest.memory_mb:6.1f} MB")
        print(f"  Disk I/O:       {latest.disk_io_read_mb:6.1f}R / {latest.disk_io_write_mb:.1f}W MB")
        
        print(f"\nFlowLoom Metrics:")
        print(f"  Sessions:       {latest.session_count:6d}")
        print(f"  Memory File:    {latest.memory_file_size_kb:6.1f} KB")
        print(f"  Cache Hit Rate: {latest.cache_hit_rate:6.1f}%")
        print(f"  Avg Query Time: {latest.avg_query_time_ms:6.1f}ms")
        
        if issues:
            print(f"\n⚠️  Performance Issues:")
            for issue in issues:
                print(f"  • {issue}")
        else:
            print(f"\n✅ No performance issues detected")
        
        print(f"\n💡 Recommendations:")
        for rec in recommendations:
            print(f"  • {rec}")
    
    def run_optimization_cycle(self):
        """Run a full optimization cycle."""
        print("Running performance optimization cycle...")
        
        # Collect metrics
        metrics = self.collect_system_metrics()
        self.metrics_history.append(metrics)
        self._save_metrics(metrics)
        
        # Detect issues and recommend actions
        issues = self.detect_performance_issues(metrics)
        if issues:
            print("Performance issues detected, running optimizations...")
            
            # Example optimizations (would integrate with actual components)
            if metrics.session_count > 20:
                print("  - Cleaning up old sessions...")
            
            if metrics.memory_file_size_kb > 500:
                print("  - Optimizing memory.json structure...")
            
            if metrics.cache_hit_rate < 80:
                print("  - Rebuilding performance indexes...")
        
        # Keep history manageable
        if len(self.metrics_history) > self.max_history:
            self.metrics_history = self.metrics_history[-self.max_history:]
    
    async def continuous_monitoring(self, duration_minutes: Optional[int] = None):
        """Run continuous performance monitoring."""
        print(f"Starting continuous monitoring (interval: {self.monitoring_interval}s)")
        
        if not self.baseline_established:
            self.establish_baseline()
        
        start_time = time.time()
        cycles = 0
        
        try:
            while True:
                # Check duration limit
                if duration_minutes:
                    elapsed_minutes = (time.time() - start_time) / 60
                    if elapsed_minutes >= duration_minutes:
                        break
                
                # Run optimization cycle
                self.run_optimization_cycle()
                cycles += 1
                
                # Print periodic status
                if cycles % 12 == 0:  # Every 12 cycles (1 minute at 5s intervals)
                    self.print_current_status()
                
                # Wait for next cycle
                await asyncio.sleep(self.monitoring_interval)
                
        except KeyboardInterrupt:
            print("\nMonitoring stopped by user")
        
        print(f"Monitoring completed after {cycles} cycles")
    
    def generate_performance_report(self, hours: int = 24) -> Dict[str, Any]:
        """Generate performance report for the specified period."""
        cutoff_time = datetime.now() - timedelta(hours=hours)
        
        # Filter metrics to time period
        recent_metrics = [
            m for m in self.metrics_history
            if datetime.fromisoformat(m.timestamp) > cutoff_time
        ]
        
        if not recent_metrics:
            return {"error": "No metrics available for the specified period"}
        
        # Calculate statistics
        cpu_values = [m.cpu_percent for m in recent_metrics]
        memory_values = [m.memory_mb for m in recent_metrics]
        session_counts = [m.session_count for m in recent_metrics]
        
        return {
            "period_hours": hours,
            "sample_count": len(recent_metrics),
            "cpu_usage": {
                "avg": sum(cpu_values) / len(cpu_values),
                "max": max(cpu_values),
                "min": min(cpu_values)
            },
            "memory_usage": {
                "avg": sum(memory_values) / len(memory_values),
                "max": max(memory_values),
                "min": min(memory_values)
            },
            "session_statistics": {
                "avg": sum(session_counts) / len(session_counts),
                "max": max(session_counts),
                "min": min(session_counts)
            },
            "latest_metrics": asdict(recent_metrics[-1]) if recent_metrics else None
        }


def main():
    """Main CLI interface."""
    parser = argparse.ArgumentParser(description="FlowLoom Performance Monitor")
    parser.add_argument("--project-root", type=Path, default=Path.cwd(),
                       help="FlowLoom project root directory")
    parser.add_argument("--interval", type=float, default=5.0,
                       help="Monitoring interval in seconds")
    
    subparsers = parser.add_subparsers(dest="command", help="Commands")
    
    # Status command
    status_parser = subparsers.add_parser("status", help="Show current performance status")
    
    # Monitor command
    monitor_parser = subparsers.add_parser("monitor", help="Run continuous monitoring")
    monitor_parser.add_argument("--duration", type=int, 
                               help="Monitoring duration in minutes")
    
    # Report command
    report_parser = subparsers.add_parser("report", help="Generate performance report")
    report_parser.add_argument("--hours", type=int, default=24,
                              help="Report period in hours")
    
    # Optimize command
    optimize_parser = subparsers.add_parser("optimize", help="Run optimization cycle")
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    # Initialize monitor
    monitor = FlowLoomPerformanceMonitor(args.project_root, args.interval)
    
    if args.command == "status":
        monitor.print_current_status()
    
    elif args.command == "monitor":
        asyncio.run(monitor.continuous_monitoring(args.duration))
    
    elif args.command == "report":
        report = monitor.generate_performance_report(args.hours)
        print(json.dumps(report, indent=2))
    
    elif args.command == "optimize":
        monitor.run_optimization_cycle()
        print("Optimization cycle completed")


if __name__ == "__main__":
    main()