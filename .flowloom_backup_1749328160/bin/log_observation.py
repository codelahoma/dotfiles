#!/usr/bin/env python3
"""Direct memory manager for JSONL memory file manipulation without MCP server"""
import json
import os
import fcntl
import tempfile
import shutil
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime
import argparse


class DirectMemoryManager:
    def __init__(self, memory_path: str = "fl-memory.json"):
        self.memory_path = Path(memory_path)
        
    def _atomic_write(self, entities: List[Dict], relations: List[Dict]) -> None:
        """Atomically write entities and relations to memory file"""
        temp_path = self.memory_path.with_suffix('.tmp')
        
        try:
            with open(temp_path, 'w') as f:
                # Acquire exclusive lock
                fcntl.flock(f.fileno(), fcntl.LOCK_EX)
                
                # Write entities first
                for entity in entities:
                    f.write(json.dumps(entity, separators=(',', ':')) + '\n')
                
                # Write relations
                for relation in relations:
                    f.write(json.dumps(relation, separators=(',', ':')) + '\n')
                
                # Ensure data is written to disk
                f.flush()
                os.fsync(f.fileno())
            
            # Atomic move
            shutil.move(str(temp_path), str(self.memory_path))
            
        except Exception as e:
            # Clean up temp file on failure
            if temp_path.exists():
                temp_path.unlink()
            raise e
    
    def _load_memory(self) -> tuple[List[Dict], List[Dict]]:
        """Load entities and relations from memory file"""
        entities = []
        relations = []
        
        if not self.memory_path.exists():
            return entities, relations
        
        with open(self.memory_path, 'r') as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                
                try:
                    data = json.loads(line)
                    if data.get('type') == 'entity':
                        entities.append(data)
                    elif data.get('type') == 'relation':
                        relations.append(data)
                except json.JSONDecodeError:
                    continue  # Skip malformed lines
        
        return entities, relations
    
    def add_entity(self, name: str, entity_type: str, observation: str) -> bool:
        """Add observation to existing entity or create new entity"""
        try:
            entities, relations = self._load_memory()
            
            # Find existing entity
            for entity in entities:
                if entity.get('name') == name and entity.get('entityType') == entity_type:
                    # Add observation if not already present
                    if observation not in entity.get('observations', []):
                        entity['observations'].append(observation)
                        self._atomic_write(entities, relations)
                        return True
                    return False  # Observation already exists
            
            # Create new entity
            new_entity = {
                'type': 'entity',
                'name': name,
                'entityType': entity_type,
                'observations': [observation]
            }
            entities.append(new_entity)
            self._atomic_write(entities, relations)
            return True
            
        except Exception as e:
            print(f"Error adding entity: {e}")
            return False
    
    def add_observation(self, name: str, entity_type: str, observation: str) -> bool:
        """Add observation to existing entity (alias for add_entity)"""
        return self.add_entity(name, entity_type, observation)
    
    def add_relation(self, from_entity: str, to_entity: str, relation_type: str) -> bool:
        """Add a relation between entities"""
        try:
            entities, relations = self._load_memory()
            
            # Check if relation already exists
            for relation in relations:
                if (relation.get('from') == from_entity and 
                    relation.get('to') == to_entity and 
                    relation.get('relationType') == relation_type):
                    return False  # Relation already exists
            
            # Add new relation
            new_relation = {
                'type': 'relation',
                'from': from_entity,
                'to': to_entity,
                'relationType': relation_type
            }
            relations.append(new_relation)
            self._atomic_write(entities, relations)
            return True
            
        except Exception as e:
            print(f"Error adding relation: {e}")
            return False
    
    def get_entity(self, name: str, entity_type: Optional[str] = None) -> Optional[Dict]:
        """Get entity by name and optionally entity type"""
        try:
            entities, _ = self._load_memory()
            
            for entity in entities:
                if entity.get('name') == name:
                    if entity_type is None or entity.get('entityType') == entity_type:
                        return entity
            return None
            
        except Exception as e:
            print(f"Error getting entity: {e}")
            return None
    
    def list_entities(self, entity_type: Optional[str] = None) -> List[Dict]:
        """List all entities, optionally filtered by type"""
        try:
            entities, _ = self._load_memory()
            
            if entity_type:
                return [e for e in entities if e.get('entityType') == entity_type]
            return entities
            
        except Exception as e:
            print(f"Error listing entities: {e}")
            return []
    
    def get_stats(self) -> Dict:
        """Get memory statistics"""
        try:
            entities, relations = self._load_memory()
            
            entity_types = {}
            total_observations = 0
            
            for entity in entities:
                etype = entity.get('entityType', 'Unknown')
                entity_types[etype] = entity_types.get(etype, 0) + 1
                total_observations += len(entity.get('observations', []))
            
            return {
                'total_entities': len(entities),
                'total_relations': len(relations),
                'total_observations': total_observations,
                'entity_types': entity_types,
                'memory_file': str(self.memory_path),
                'file_exists': self.memory_path.exists(),
                'file_size_bytes': self.memory_path.stat().st_size if self.memory_path.exists() else 0
            }
            
        except Exception as e:
            print(f"Error getting stats: {e}")
            return {}


def _get_observation_text(args) -> Optional[str]:
    """Get observation text from file or command line argument"""
    if args.from_file:
        try:
            with open(args.from_file, 'r', encoding='utf-8') as f:
                return f.read().strip()
        except FileNotFoundError:
            print(f"Error: File '{args.from_file}' not found")
            return None
        except Exception as e:
            print(f"Error reading file '{args.from_file}': {e}")
            return None
    elif args.observation:
        return args.observation
    else:
        print("Error: Must provide either observation text or --from-file option")
        return None


def main():
    parser = argparse.ArgumentParser(
        description='Direct memory manager for FlowLoom - Manages entities, observations, and relationships in fl-memory.json',
        epilog='''
Examples:
  # Add a new entity with observation
  %(prog)s add-entity "FlowLoom Memory System" "Feature" "Implemented JSONL format for clean git merges"
  
  # Add observation to existing entity
  %(prog)s add-observation "Session-12345" "Session" "Completed authentication implementation"
  
  # Add observation for complex text from file
  %(prog)s add-observation "Architecture Decision" "Documentation" --from-file decision.md
  
  # Create a relationship
  %(prog)s add-relation "Authentication Module" "Database Layer" "depends_on"
  
  # Query entities
  %(prog)s get-entity "FlowLoom Memory System"
  %(prog)s list-entities --type Feature
  %(prog)s stats

Common entity types: Feature, Component, Session, Task, Documentation, System, User
Common relation types: depends_on, implements, contains, relates_to, creates, uses
        ''',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument('--memory-file', default='fl-memory.json', 
                       help='Path to memory file (default: fl-memory.json)')
    subparsers = parser.add_subparsers(dest='command', help='Available commands', 
                                      metavar='command')
    
    # Add entity command
    add_entity_parser = subparsers.add_parser('add-entity', 
        help='Create new entity or update existing with observation',
        description='Create a new entity with an initial observation, or add observation to existing entity')
    add_entity_parser.add_argument('name', help='Unique entity name (e.g., "Authentication System")')
    add_entity_parser.add_argument('type', help='Entity type (e.g., Feature, Component, Session)')
    add_entity_parser.add_argument('observation', nargs='?', 
                                  help='Observation text (quote if contains spaces)')
    add_entity_parser.add_argument('--from-file', 
                                  help='Read observation from file (for long/complex text)')
    
    # Add observation command
    add_obs_parser = subparsers.add_parser('add-observation', 
        help='Add observation to existing entity',
        description='Add a new observation to an existing entity. Creates entity if not found.')
    add_obs_parser.add_argument('name', help='Entity name to add observation to')
    add_obs_parser.add_argument('type', help='Entity type (creates if entity not found)')
    add_obs_parser.add_argument('observation', nargs='?', 
                               help='Observation text (quote if contains spaces)')
    add_obs_parser.add_argument('--from-file', 
                               help='Read observation from file (for long/complex text)')
    
    # Add relation command
    add_rel_parser = subparsers.add_parser('add-relation', 
        help='Create relationship between two entities',
        description='Add a directed relationship from one entity to another')
    add_rel_parser.add_argument('from_entity', help='Source entity name')
    add_rel_parser.add_argument('to_entity', help='Target entity name')
    add_rel_parser.add_argument('relation_type', 
                               help='Type of relationship (e.g., depends_on, implements, contains)')
    
    # Get entity command
    get_entity_parser = subparsers.add_parser('get-entity', 
        help='Retrieve entity details by name',
        description='Get full details of an entity including all observations')
    get_entity_parser.add_argument('name', help='Entity name to retrieve')
    get_entity_parser.add_argument('--type', help='Filter by entity type (optional)')
    
    # List entities command
    list_parser = subparsers.add_parser('list-entities', 
        help='List all entities or filter by type',
        description='Show summary of all entities in memory')
    list_parser.add_argument('--type', help='Filter by entity type (e.g., Feature, Session)')
    
    # Stats command
    stats_parser = subparsers.add_parser('stats', 
        help='Show memory statistics and summary',
        description='Display statistics about entities, relations, and observations')
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    manager = DirectMemoryManager(args.memory_file)
    
    if args.command == 'add-entity':
        # Get observation from file or command line
        observation = _get_observation_text(args)
        if observation is None:
            return
            
        result = manager.add_entity(args.name, args.type, observation)
        if result:
            print(f"✓ Entity '{args.name}' ({args.type}) added/updated")
            print(f"  Observation: {observation[:100]}{'...' if len(observation) > 100 else ''}")
        else:
            print(f"• Entity '{args.name}' unchanged (observation already exists)")
        
    elif args.command == 'add-observation':
        # Get observation from file or command line
        observation = _get_observation_text(args)
        if observation is None:
            return
            
        result = manager.add_observation(args.name, args.type, observation)
        if result:
            print(f"✓ Observation added to '{args.name}' ({args.type})")
            print(f"  Content: {observation[:100]}{'...' if len(observation) > 100 else ''}")
        else:
            print(f"• Observation unchanged for '{args.name}' (already exists)")
        
    elif args.command == 'add-relation':
        result = manager.add_relation(args.from_entity, args.to_entity, args.relation_type)
        if result:
            print(f"✓ Relation added: {args.from_entity} --[{args.relation_type}]--> {args.to_entity}")
        else:
            print(f"• Relation unchanged (already exists)")
        
    elif args.command == 'get-entity':
        entity = manager.get_entity(args.name, args.type)
        if entity:
            print(json.dumps(entity, indent=2))
        else:
            print("Entity not found")
            
    elif args.command == 'list-entities':
        entities = manager.list_entities(args.type)
        for entity in entities:
            print(f"{entity['name']} ({entity['entityType']}) - {len(entity.get('observations', []))} observations")
            
    elif args.command == 'stats':
        stats = manager.get_stats()
        print("=== FlowLoom Memory Statistics ===")
        print(f"📊 Total entities:      {stats.get('total_entities', 0):,}")
        print(f"🔗 Total relations:     {stats.get('total_relations', 0):,}")
        print(f"📝 Total observations:  {stats.get('total_observations', 0):,}")
        print(f"📁 Memory file:         {stats.get('memory_file', 'unknown')}")
        
        file_size = stats.get('file_size_bytes', 0)
        if file_size > 1024 * 1024:
            print(f"💾 File size:           {file_size / (1024 * 1024):.1f} MB")
        else:
            print(f"💾 File size:           {file_size / 1024:.1f} KB")
        
        print("\n📋 Entity type distribution:")
        entity_types = stats.get('entity_types', {})
        if entity_types:
            max_count = max(entity_types.values())
            for etype, count in sorted(entity_types.items(), key=lambda x: x[1], reverse=True):
                bar_length = int(count * 30 / max_count) if max_count > 0 else 0
                bar = '█' * bar_length
                print(f"  {etype:<20} {count:>5} {bar}")


if __name__ == "__main__":
    main()