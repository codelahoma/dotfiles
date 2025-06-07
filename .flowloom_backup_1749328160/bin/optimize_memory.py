#!/usr/bin/env python3
"""
FlowLoom Memory Optimization Utility

Optimizes memory.json structure, builds performance indexes,
and provides memory coordination performance improvements.
"""

import json
import time
import hashlib
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any, Set, Tuple
from collections import defaultdict
import argparse
import sys
import shutil


class MemoryOptimizer:
    """Advanced memory.json optimization and indexing."""
    
    def __init__(self, memory_file: Path):
        self.memory_file = memory_file
        self.backup_dir = memory_file.parent / ".flowloom" / "memory_backups"
        self.backup_dir.mkdir(parents=True, exist_ok=True)
        
        # Performance tracking
        self.start_time = time.time()
        self.operations_log = []
    
    def log_operation(self, operation: str, details: Dict[str, Any]):
        """Log optimization operation."""
        self.operations_log.append({
            "timestamp": datetime.now().isoformat(),
            "operation": operation,
            "details": details,
            "elapsed_ms": (time.time() - self.start_time) * 1000
        })
    
    def create_backup(self) -> Path:
        """Create backup of memory.json before optimization."""
        if not self.memory_file.exists():
            raise FileNotFoundError(f"Memory file not found: {self.memory_file}")
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_file = self.backup_dir / f"memory_backup_{timestamp}.json"
        
        shutil.copy2(self.memory_file, backup_file)
        
        self.log_operation("backup_created", {
            "backup_file": str(backup_file),
            "original_size": self.memory_file.stat().st_size
        })
        
        return backup_file
    
    def analyze_structure(self) -> Dict[str, Any]:
        """Analyze memory.json structure and identify optimization opportunities."""
        if not self.memory_file.exists():
            return {"error": "Memory file not found"}
        
        start_time = time.time()
        
        try:
            content = self.memory_file.read_text()
            lines = content.strip().split('\n')
            
            entities = {}
            relations = []
            duplicates = defaultdict(list)
            orphaned_relations = []
            entity_types = defaultdict(int)
            observation_stats = {"total": 0, "avg_per_entity": 0, "max_per_entity": 0}
            
            # Parse all objects
            for line_num, line in enumerate(lines, 1):
                if not line.strip():
                    continue
                
                try:
                    obj = json.loads(line)
                    
                    if obj.get('type') == 'entity':
                        name = obj.get('name')
                        entity_type = obj.get('entityType')
                        observations = obj.get('observations', [])
                        
                        if name:
                            if name in entities:
                                duplicates[name].append(line_num)
                            else:
                                entities[name] = {
                                    "line": line_num,
                                    "type": entity_type,
                                    "observation_count": len(observations)
                                }
                                
                                # Track entity type distribution
                                entity_types[entity_type] += 1
                                
                                # Track observation statistics
                                observation_stats["total"] += len(observations)
                                observation_stats["max_per_entity"] = max(
                                    observation_stats["max_per_entity"], 
                                    len(observations)
                                )
                    
                    elif obj.get('type') == 'relation':
                        from_entity = obj.get('from')
                        to_entity = obj.get('to')
                        relation_type = obj.get('relationType')
                        
                        relations.append({
                            "line": line_num,
                            "from": from_entity,
                            "to": to_entity,
                            "type": relation_type
                        })
                        
                        # Check for orphaned relations
                        if from_entity not in entities and to_entity not in entities:
                            orphaned_relations.append(line_num)
                
                except json.JSONDecodeError:
                    continue
            
            # Calculate averages
            if entities:
                observation_stats["avg_per_entity"] = observation_stats["total"] / len(entities)
            
            analysis_time = (time.time() - start_time) * 1000
            
            analysis = {
                "file_stats": {
                    "size_bytes": self.memory_file.stat().st_size,
                    "line_count": len(lines),
                    "entity_count": len(entities),
                    "relation_count": len(relations)
                },
                "optimization_opportunities": {
                    "duplicate_entities": len(duplicates),
                    "orphaned_relations": len(orphaned_relations),
                    "total_duplicates": sum(len(dups) for dups in duplicates.values())
                },
                "entity_type_distribution": dict(entity_types),
                "observation_statistics": observation_stats,
                "duplicate_details": dict(duplicates),
                "orphaned_relation_lines": orphaned_relations,
                "analysis_time_ms": round(analysis_time, 2)
            }
            
            self.log_operation("structure_analysis", analysis)
            return analysis
            
        except Exception as e:
            return {"error": str(e)}
    
    def optimize_structure(self, merge_duplicates: bool = True, 
                          remove_orphans: bool = True) -> Dict[str, Any]:
        """Optimize memory.json structure by removing duplicates and orphans."""
        
        # Create backup first
        backup_file = self.create_backup()
        
        try:
            content = self.memory_file.read_text()
            lines = content.strip().split('\n')
            
            entities = {}
            relations = []
            optimized_entities = {}
            optimization_stats = {
                "entities_merged": 0,
                "relations_removed": 0,
                "observations_merged": 0,
                "size_reduction_bytes": 0
            }
            
            # First pass: collect and merge entities
            for line in lines:
                if not line.strip():
                    continue
                
                try:
                    obj = json.loads(line)
                    
                    if obj.get('type') == 'entity':
                        name = obj.get('name')
                        if not name:
                            continue
                        
                        if name in optimized_entities:
                            if merge_duplicates:
                                # Merge observations
                                existing_obs = set(optimized_entities[name].get('observations', []))
                                new_obs = set(obj.get('observations', []))
                                merged_obs = list(existing_obs | new_obs)
                                optimized_entities[name]['observations'] = merged_obs
                                
                                optimization_stats["entities_merged"] += 1
                                optimization_stats["observations_merged"] += len(new_obs - existing_obs)
                        else:
                            optimized_entities[name] = obj
                    
                    elif obj.get('type') == 'relation':
                        relations.append(obj)
                
                except json.JSONDecodeError:
                    continue
            
            # Second pass: filter orphaned relations
            valid_relations = []
            entity_names = set(optimized_entities.keys())
            
            for relation in relations:
                from_entity = relation.get('from')
                to_entity = relation.get('to')
                
                if remove_orphans:
                    if from_entity in entity_names or to_entity in entity_names:
                        valid_relations.append(relation)
                    else:
                        optimization_stats["relations_removed"] += 1
                else:
                    valid_relations.append(relation)
            
            # Write optimized content
            optimized_lines = []
            
            # Add entities
            for entity in optimized_entities.values():
                optimized_lines.append(json.dumps(entity))
            
            # Add relations
            for relation in valid_relations:
                optimized_lines.append(json.dumps(relation))
            
            optimized_content = '\n'.join(optimized_lines) + '\n'
            
            # Calculate size reduction
            original_size = len(content)
            optimized_size = len(optimized_content)
            optimization_stats["size_reduction_bytes"] = original_size - optimized_size
            
            # Write optimized file
            self.memory_file.write_text(optimized_content)
            
            result = {
                "success": True,
                "backup_file": str(backup_file),
                "optimization_stats": optimization_stats,
                "final_stats": {
                    "entities": len(optimized_entities),
                    "relations": len(valid_relations),
                    "size_bytes": optimized_size,
                    "size_reduction_percent": round(
                        (optimization_stats["size_reduction_bytes"] / original_size) * 100, 2
                    ) if original_size > 0 else 0
                }
            }
            
            self.log_operation("structure_optimization", result)
            return result
            
        except Exception as e:
            # Restore backup on error
            if backup_file.exists():
                shutil.copy2(backup_file, self.memory_file)
            
            return {"success": False, "error": str(e), "backup_restored": True}
    
    def build_performance_indexes(self) -> Dict[str, Any]:
        """Build performance indexes for faster querying."""
        start_time = time.time()
        
        index_dir = self.memory_file.parent / ".flowloom" / "indexes"
        index_dir.mkdir(parents=True, exist_ok=True)
        
        try:
            content = self.memory_file.read_text()
            lines = content.strip().split('\n')
            
            # Build various indexes
            entity_index = {}           # name -> line number
            type_index = defaultdict(list)  # entityType -> [names]
            observation_index = defaultdict(set)  # keyword -> {entity_names}
            relation_index = defaultdict(list)    # entity_name -> [relations]
            
            for line_num, line in enumerate(lines):
                if not line.strip():
                    continue
                
                try:
                    obj = json.loads(line)
                    
                    if obj.get('type') == 'entity':
                        name = obj.get('name')
                        entity_type = obj.get('entityType')
                        observations = obj.get('observations', [])
                        
                        if name:
                            entity_index[name] = line_num
                            
                            if entity_type:
                                type_index[entity_type].append(name)
                            
                            # Index observation keywords
                            for obs in observations:
                                words = obs.lower().split()
                                for word in words:
                                    if len(word) > 3:  # Index meaningful words
                                        observation_index[word].add(name)
                    
                    elif obj.get('type') == 'relation':
                        from_entity = obj.get('from')
                        to_entity = obj.get('to')
                        relation_type = obj.get('relationType')
                        
                        if from_entity:
                            relation_index[from_entity].append({
                                "to": to_entity,
                                "type": relation_type,
                                "line": line_num
                            })
                        
                        if to_entity and to_entity != from_entity:
                            relation_index[to_entity].append({
                                "from": from_entity,
                                "type": relation_type,
                                "line": line_num
                            })
                
                except json.JSONDecodeError:
                    continue
            
            # Save indexes
            indexes = {
                "entity_index": entity_index,
                "type_index": dict(type_index),
                "observation_index": {k: list(v) for k, v in observation_index.items()},
                "relation_index": dict(relation_index),
                "metadata": {
                    "created": datetime.now().isoformat(),
                    "entity_count": len(entity_index),
                    "type_count": len(type_index),
                    "observation_keywords": len(observation_index),
                    "relations_indexed": sum(len(rels) for rels in relation_index.values())
                }
            }
            
            index_file = index_dir / "memory_indexes.json"
            index_file.write_text(json.dumps(indexes, indent=2))
            
            build_time = (time.time() - start_time) * 1000
            
            result = {
                "success": True,
                "index_file": str(index_file),
                "indexes_built": {
                    "entity_index": len(entity_index),
                    "type_index": len(type_index),
                    "observation_keywords": len(observation_index),
                    "relation_mappings": len(relation_index)
                },
                "build_time_ms": round(build_time, 2)
            }
            
            self.log_operation("index_building", result)
            return result
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    def validate_integrity(self) -> Dict[str, Any]:
        """Validate memory.json integrity and consistency."""
        start_time = time.time()
        
        try:
            content = self.memory_file.read_text()
            lines = content.strip().split('\n')
            
            validation_results = {
                "valid_lines": 0,
                "invalid_lines": 0,
                "entities": 0,
                "relations": 0,
                "issues": []
            }
            
            entity_names = set()
            relation_entities = set()
            
            for line_num, line in enumerate(lines, 1):
                if not line.strip():
                    continue
                
                try:
                    obj = json.loads(line)
                    validation_results["valid_lines"] += 1
                    
                    if obj.get('type') == 'entity':
                        validation_results["entities"] += 1
                        name = obj.get('name')
                        
                        if not name:
                            validation_results["issues"].append(
                                f"Line {line_num}: Entity missing name"
                            )
                        else:
                            entity_names.add(name)
                        
                        if not obj.get('entityType'):
                            validation_results["issues"].append(
                                f"Line {line_num}: Entity '{name}' missing entityType"
                            )
                    
                    elif obj.get('type') == 'relation':
                        validation_results["relations"] += 1
                        from_entity = obj.get('from')
                        to_entity = obj.get('to')
                        
                        if from_entity:
                            relation_entities.add(from_entity)
                        if to_entity:
                            relation_entities.add(to_entity)
                        
                        if not from_entity or not to_entity:
                            validation_results["issues"].append(
                                f"Line {line_num}: Relation missing from/to entities"
                            )
                        
                        if not obj.get('relationType'):
                            validation_results["issues"].append(
                                f"Line {line_num}: Relation missing relationType"
                            )
                    
                    else:
                        validation_results["issues"].append(
                            f"Line {line_num}: Unknown object type '{obj.get('type')}'"
                        )
                
                except json.JSONDecodeError as e:
                    validation_results["invalid_lines"] += 1
                    validation_results["issues"].append(
                        f"Line {line_num}: JSON parse error - {e}"
                    )
            
            # Check for orphaned relation references
            orphaned_refs = relation_entities - entity_names
            for ref in orphaned_refs:
                validation_results["issues"].append(
                    f"Orphaned relation reference: '{ref}' (no matching entity)"
                )
            
            validation_time = (time.time() - start_time) * 1000
            
            validation_results.update({
                "validation_time_ms": round(validation_time, 2),
                "is_valid": len(validation_results["issues"]) == 0,
                "orphaned_references": len(orphaned_refs)
            })
            
            self.log_operation("integrity_validation", validation_results)
            return validation_results
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    def cleanup_old_backups(self, keep_latest: int = 10) -> Dict[str, Any]:
        """Clean up old backup files, keeping only the latest N backups."""
        try:
            backup_files = list(self.backup_dir.glob("memory_backup_*.json"))
            backup_files.sort(key=lambda f: f.stat().st_mtime, reverse=True)
            
            if len(backup_files) <= keep_latest:
                return {
                    "cleaned": 0,
                    "remaining": len(backup_files),
                    "message": f"Only {len(backup_files)} backups found, keeping all"
                }
            
            # Remove old backups
            removed_count = 0
            for backup_file in backup_files[keep_latest:]:
                try:
                    backup_file.unlink()
                    removed_count += 1
                except Exception:
                    continue
            
            result = {
                "cleaned": removed_count,
                "remaining": len(backup_files) - removed_count,
                "kept_latest": keep_latest
            }
            
            self.log_operation("backup_cleanup", result)
            return result
            
        except Exception as e:
            return {"error": str(e)}
    
    def get_operations_log(self) -> List[Dict[str, Any]]:
        """Get log of all optimization operations performed."""
        return self.operations_log.copy()


def main():
    """Main CLI interface."""
    parser = argparse.ArgumentParser(description="FlowLoom Memory Optimization Utility")
    parser.add_argument("--memory-file", type=Path, default=Path("memory.json"),
                       help="Path to memory.json file")
    
    subparsers = parser.add_subparsers(dest="command", help="Commands")
    
    # Analyze command
    analyze_parser = subparsers.add_parser("analyze", help="Analyze memory structure")
    
    # Optimize command
    optimize_parser = subparsers.add_parser("optimize", help="Optimize memory structure")
    optimize_parser.add_argument("--no-merge-duplicates", action="store_true",
                                help="Don't merge duplicate entities")
    optimize_parser.add_argument("--no-remove-orphans", action="store_true",
                                help="Don't remove orphaned relations")
    
    # Index command
    index_parser = subparsers.add_parser("index", help="Build performance indexes")
    
    # Validate command
    validate_parser = subparsers.add_parser("validate", help="Validate memory integrity")
    
    # Cleanup command
    cleanup_parser = subparsers.add_parser("cleanup", help="Clean up old backups")
    cleanup_parser.add_argument("--keep", type=int, default=10,
                               help="Number of latest backups to keep")
    
    # Full optimization command
    full_parser = subparsers.add_parser("full", help="Run full optimization cycle")
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    # Initialize optimizer
    optimizer = MemoryOptimizer(args.memory_file)
    
    if args.command == "analyze":
        result = optimizer.analyze_structure()
        print(json.dumps(result, indent=2))
    
    elif args.command == "optimize":
        result = optimizer.optimize_structure(
            merge_duplicates=not args.no_merge_duplicates,
            remove_orphans=not args.no_remove_orphans
        )
        print(json.dumps(result, indent=2))
    
    elif args.command == "index":
        result = optimizer.build_performance_indexes()
        print(json.dumps(result, indent=2))
    
    elif args.command == "validate":
        result = optimizer.validate_integrity()
        print(json.dumps(result, indent=2))
    
    elif args.command == "cleanup":
        result = optimizer.cleanup_old_backups(args.keep)
        print(json.dumps(result, indent=2))
    
    elif args.command == "full":
        print("Running full optimization cycle...")
        
        # Step 1: Analyze
        print("\n1. Analyzing structure...")
        analysis = optimizer.analyze_structure()
        print(f"   Found {analysis.get('optimization_opportunities', {}).get('duplicate_entities', 0)} duplicate entities")
        
        # Step 2: Validate
        print("\n2. Validating integrity...")
        validation = optimizer.validate_integrity()
        print(f"   Found {len(validation.get('issues', []))} integrity issues")
        
        # Step 3: Optimize
        print("\n3. Optimizing structure...")
        optimization = optimizer.optimize_structure()
        if optimization.get('success'):
            stats = optimization['optimization_stats']
            print(f"   Merged {stats['entities_merged']} duplicate entities")
            print(f"   Removed {stats['relations_removed']} orphaned relations")
            print(f"   Reduced size by {stats['size_reduction_bytes']} bytes")
        
        # Step 4: Build indexes
        print("\n4. Building performance indexes...")
        indexing = optimizer.build_performance_indexes()
        if indexing.get('success'):
            print(f"   Built {len(indexing['indexes_built'])} index types")
        
        # Step 5: Cleanup
        print("\n5. Cleaning up old backups...")
        cleanup = optimizer.cleanup_old_backups()
        print(f"   Cleaned {cleanup.get('cleaned', 0)} old backup files")
        
        print("\n✅ Full optimization cycle completed!")
        
        # Show operations log
        print("\nOperations performed:")
        for op in optimizer.get_operations_log():
            print(f"  - {op['operation']}: {op['elapsed_ms']:.1f}ms")


if __name__ == "__main__":
    main()