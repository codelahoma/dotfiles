#!/usr/bin/env python3
"""Memory reconciliation engine for comprehensive JSONL merging"""
import json
import hashlib
from pathlib import Path
from typing import List, Dict, Set, Tuple
from datetime import datetime
import argparse

class JSONLParser:
    def __init__(self):
        self.entities = []
        self.relations = []
        self.errors = []
    
    def parse_content(self, content: str, source: str) -> Tuple[List[Dict], List[Dict], List[str]]:
        """Parse JSONL content and return entities, relations, and errors"""
        entities = []
        relations = []
        errors = []
        
        for line_num, line in enumerate(content.split('\n'), 1):
            if not line.strip():
                continue
            
            try:
                data = json.loads(line)
                if not isinstance(data, dict):
                    errors.append(f"{source}:{line_num}: Not a JSON object")
                    continue
                    
                if self.validate_entry(data):
                    # Add source metadata
                    data['_source'] = source
                    data['_line'] = line_num
                    
                    if data.get('type') == 'entity':
                        entities.append(data)
                    elif data.get('type') == 'relation':
                        relations.append(data)
                else:
                    errors.append(f"{source}:{line_num}: Invalid schema")
            except json.JSONDecodeError as e:
                errors.append(f"{source}:{line_num}: JSON decode error - {str(e)}")
            except Exception as e:
                errors.append(f"{source}:{line_num}: Unexpected error - {str(e)}")
        
        return entities, relations, errors
    
    def validate_entry(self, data: Dict) -> bool:
        """Validate entity or relation schema"""
        if data.get('type') == 'entity':
            return all(key in data for key in ['name', 'entityType', 'observations'])
        elif data.get('type') == 'relation':
            return all(key in data for key in ['from', 'to', 'relationType'])
        return False

class EntityMerger:
    def __init__(self):
        self.merge_stats = {
            'total_entities': 0,
            'merged_entities': 0,
            'unique_entities': 0,
            'observations_merged': 0
        }
    
    def merge_entities(self, entities: List[Dict]) -> List[Dict]:
        """Merge entities with same name/entityType"""
        print(f"🔗 Merging {len(entities)} entities...")
        
        entity_groups = {}
        
        # Group entities by identity (name + entityType)
        for entity in entities:
            key = self.get_entity_key(entity)
            if key not in entity_groups:
                entity_groups[key] = []
            entity_groups[key].append(entity)
        
        self.merge_stats['total_entities'] = len(entities)
        
        merged_entities = []
        for (name, entity_type), group in entity_groups.items():
            if len(group) == 1:
                # Single entity, just clean it up
                clean_entity = self.clean_entity(group[0])
                merged_entities.append(clean_entity)
                self.merge_stats['unique_entities'] += 1
            else:
                # Multiple entities with same identity - merge them
                merged = self.merge_entity_group(group)
                merged_entities.append(merged)
                self.merge_stats['merged_entities'] += 1
                print(f"  📦 Merged {len(group)} versions of '{name}' ({entity_type})")
        
        print(f"  ✅ Result: {len(merged_entities)} unique entities")
        return merged_entities
    
    def get_entity_key(self, entity: Dict) -> Tuple[str, str]:
        """Get unique key for entity identity"""
        return (entity.get('name', ''), entity.get('entityType', ''))
    
    def merge_entity_group(self, entities: List[Dict]) -> Dict:
        """Merge multiple entities with same identity"""
        base_entity = entities[0].copy()
        all_observations = []
        source_info = []
        
        # Collect all observations and source info
        for entity in entities:
            obs = entity.get('observations', [])
            if isinstance(obs, list):
                all_observations.extend(obs)
            
            # Track source information
            source = entity.get('_source', 'unknown')
            line = entity.get('_line', 0)
            source_info.append(f"{source}:{line}")
        
        # Deduplicate observations while preserving order and Shell_ID chronology
        unique_observations = self.deduplicate_observations(all_observations)
        
        # Create merged entity
        merged_entity = {
            'type': 'entity',
            'name': base_entity.get('name'),
            'entityType': base_entity.get('entityType'),
            'observations': unique_observations
        }
        
        self.merge_stats['observations_merged'] += len(all_observations) - len(unique_observations)
        
        return merged_entity
    
    def deduplicate_observations(self, observations: List[str]) -> List[str]:
        """Deduplicate observations while preserving chronological order"""
        seen = set()
        unique_observations = []
        
        # Sort by Shell_ID timestamp if present
        def get_timestamp(obs):
            if 'Shell_ID:' in obs and ' - ' in obs:
                try:
                    parts = obs.split(' - ')
                    if len(parts) > 1:
                        timestamp_part = parts[1]
                        if 'T' in timestamp_part and 'Z' in timestamp_part:
                            return timestamp_part.split(' |')[0]
                except:
                    pass
            return "0000-00-00T00:00:00Z"
        
        # Sort observations by timestamp
        sorted_observations = sorted(observations, key=get_timestamp)
        
        for obs in sorted_observations:
            # Create normalized version for deduplication
            normalized = obs.strip()
            if normalized and normalized not in seen:
                unique_observations.append(obs)
                seen.add(normalized)
        
        return unique_observations
    
    def clean_entity(self, entity: Dict) -> Dict:
        """Clean up a single entity"""
        return {
            'type': 'entity',
            'name': entity.get('name'),
            'entityType': entity.get('entityType'),
            'observations': entity.get('observations', [])
        }

class RelationDeduplicator:
    def __init__(self):
        self.dedupe_stats = {
            'total_relations': 0,
            'unique_relations': 0,
            'duplicates_removed': 0
        }
    
    def deduplicate_relations(self, relations: List[Dict]) -> List[Dict]:
        """Deduplicate relations"""
        print(f"🔗 Deduplicating {len(relations)} relations...")
        
        self.dedupe_stats['total_relations'] = len(relations)
        
        unique_relations = []
        seen_relations = set()
        
        for relation in relations:
            relation_key = self.get_relation_key(relation)
            if relation_key not in seen_relations:
                unique_relations.append(self.clean_relation(relation))
                seen_relations.add(relation_key)
            else:
                self.dedupe_stats['duplicates_removed'] += 1
        
        self.dedupe_stats['unique_relations'] = len(unique_relations)
        print(f"  ✅ Result: {len(unique_relations)} unique relations")
        
        return unique_relations
    
    def get_relation_key(self, relation: Dict) -> Tuple[str, str, str]:
        """Get unique key for relation"""
        return (
            relation.get('from', ''),
            relation.get('to', ''),
            relation.get('relationType', '')
        )
    
    def clean_relation(self, relation: Dict) -> Dict:
        """Clean up a relation"""
        return {
            'type': 'relation',
            'from': relation.get('from'),
            'to': relation.get('to'),
            'relationType': relation.get('relationType')
        }

class MemoryReconciler:
    def __init__(self):
        self.parser = JSONLParser()
        self.merger = EntityMerger()
        self.deduplicator = RelationDeduplicator()
        self.reconciliation_stats = {
            'total_sources': 0,
            'valid_sources': 0,
            'invalid_sources': 0,
            'total_size_bytes': 0,
            'start_time': datetime.now().isoformat()
        }
    
    def reconcile_from_discovery(self, discovery_file: Path) -> Dict:
        """Reconcile memory from discovery results"""
        print("🚀 Starting comprehensive memory reconciliation...")
        
        with open(discovery_file, 'r') as f:
            memory_sources = json.load(f)
        
        all_entities = []
        all_relations = []
        all_errors = []
        
        # Process each category of sources
        for category, sources in memory_sources.items():
            if not sources:
                continue
                
            print(f"\n📂 Processing {category.upper()} ({len(sources)} sources)")
            
            for source in sources:
                self.reconciliation_stats['total_sources'] += 1
                source_name = source.get('source', 'unknown')
                
                try:
                    content = self.get_content_from_source(source)
                    if content:
                        entities, relations, errors = self.parser.parse_content(content, source_name)
                        
                        all_entities.extend(entities)
                        all_relations.extend(relations)
                        all_errors.extend([f"{source_name}: {error}" for error in errors])
                        
                        if errors:
                            self.reconciliation_stats['invalid_sources'] += 1
                            print(f"  ⚠️  {source_name}: {len(errors)} errors")
                        else:
                            self.reconciliation_stats['valid_sources'] += 1
                            
                        print(f"  ✅ {source_name}: {len(entities)} entities, {len(relations)} relations")
                        
                        if 'size' in source:
                            self.reconciliation_stats['total_size_bytes'] += source['size']
                
                except Exception as e:
                    all_errors.append(f"{source_name}: Failed to process - {str(e)}")
                    self.reconciliation_stats['invalid_sources'] += 1
                    print(f"  ❌ {source_name}: {str(e)}")
        
        print(f"\n📊 Raw extraction complete:")
        print(f"   Entities: {len(all_entities)}")
        print(f"   Relations: {len(all_relations)}")
        print(f"   Errors: {len(all_errors)}")
        
        # Merge and deduplicate
        print(f"\n🔄 Starting reconciliation process...")
        merged_entities = self.merger.merge_entities(all_entities)
        unique_relations = self.deduplicator.deduplicate_relations(all_relations)
        
        self.reconciliation_stats['end_time'] = datetime.now().isoformat()
        
        return {
            'entities': merged_entities,
            'relations': unique_relations,
            'errors': all_errors,
            'stats': {
                **self.reconciliation_stats,
                'merger_stats': self.merger.merge_stats,
                'deduplication_stats': self.deduplicator.dedupe_stats,
                'final_entities': len(merged_entities),
                'final_relations': len(unique_relations),
                'total_errors': len(all_errors)
            }
        }
    
    def get_content_from_source(self, source: Dict) -> str:
        """Get content from a source (file or git-based)"""
        if 'path' in source:
            # File-based source
            try:
                with open(source['path'], 'r') as f:
                    return f.read()
            except Exception as e:
                raise Exception(f"Cannot read file {source['path']}: {e}")
        elif 'content' in source:
            # Git-based source
            return source['content']
        else:
            raise Exception("Source has neither path nor content")
    
    def write_fl_memory(self, reconciled_data: Dict, output_path: Path) -> None:
        """Write reconciled data to fl-memory.json"""
        print(f"\n💾 Writing reconciled memory to {output_path}...")
        
        with open(output_path, 'w') as f:
            # Write entities first
            for entity in reconciled_data['entities']:
                f.write(json.dumps(entity, separators=(',', ':')) + '\n')
            
            # Write relations
            for relation in reconciled_data['relations']:
                f.write(json.dumps(relation, separators=(',', ':')) + '\n')
        
        # Write stats to companion file
        stats_path = output_path.with_suffix('.stats.json')
        with open(stats_path, 'w') as f:
            json.dump(reconciled_data['stats'], f, indent=2)
        
        print(f"✅ Reconciliation complete!")
        print(f"   Entities: {len(reconciled_data['entities'])}")
        print(f"   Relations: {len(reconciled_data['relations'])}")
        print(f"   Stats: {stats_path}")

def main():
    parser = argparse.ArgumentParser(description='Reconcile all memory sources')
    parser.add_argument('discovery_file', help='Discovery results JSON file')
    parser.add_argument('--output', '-o', default='fl-memory.json', help='Output file')
    parser.add_argument('--errors-output', help='Output file for errors')
    args = parser.parse_args()
    
    reconciler = MemoryReconciler()
    
    # Perform reconciliation
    reconciled_data = reconciler.reconcile_from_discovery(Path(args.discovery_file))
    
    # Write results
    reconciler.write_fl_memory(reconciled_data, Path(args.output))
    
    # Write errors if requested
    if args.errors_output and reconciled_data['errors']:
        with open(args.errors_output, 'w') as f:
            for error in reconciled_data['errors']:
                f.write(error + '\n')
        print(f"📝 Errors written to {args.errors_output}")
    
    print(f"\n🎉 Memory reconciliation completed successfully!")
    print(f"   Final result: {args.output}")
    print(f"   Sources processed: {reconciled_data['stats']['total_sources']}")
    print(f"   Valid sources: {reconciled_data['stats']['valid_sources']}")
    print(f"   Final entities: {reconciled_data['stats']['final_entities']}")
    print(f"   Final relations: {reconciled_data['stats']['final_relations']}")

if __name__ == "__main__":
    main()