#!/usr/bin/env python3
"""Migration utility to convert string-based observations to JSON objects with timestamps"""
import json
import shutil
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional
import argparse


def migrate_memory_file(memory_path: str, backup: bool = True) -> bool:
    """
    Migrate string-based observations to JSON objects with timestamps.
    
    Args:
        memory_path: Path to the fl-memory.json file
        backup: Whether to create a backup before migration
    
    Returns:
        True if migration successful, False otherwise
    """
    memory_file = Path(memory_path)
    
    if not memory_file.exists():
        print(f"❌ Memory file {memory_path} does not exist")
        return False
    
    # Create backup if requested
    if backup:
        backup_path = memory_file.with_suffix(f'.backup.{int(datetime.now().timestamp())}')
        shutil.copy2(memory_file, backup_path)
        print(f"📋 Backup created: {backup_path}")
    
    # Load existing data
    entities = []
    relations = []
    migration_count = 0
    
    with open(memory_file, 'r') as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            
            try:
                data = json.loads(line)
                
                if data.get('type') == 'entity':
                    # Migrate observations if they're strings
                    observations = data.get('observations', [])
                    migrated_observations = []
                    
                    for i, obs in enumerate(observations):
                        if isinstance(obs, str):
                            # Convert string to JSON object
                            # Use a retroactive timestamp (spread over past week for variety)
                            base_time = datetime.utcnow() - timedelta(days=7)
                            retroactive_time = base_time + timedelta(hours=i * 2)  # Space them out
                            
                            migrated_obs = {
                                "timestamp": retroactive_time.isoformat() + "Z",
                                "content": obs,
                                "tags": _infer_tags_from_content(obs)
                            }
                            migrated_observations.append(migrated_obs)
                            migration_count += 1
                        else:
                            # Already a JSON object, keep as-is
                            migrated_observations.append(obs)
                    
                    data['observations'] = migrated_observations
                    entities.append(data)
                    
                elif data.get('type') == 'relation':
                    relations.append(data)
                    
            except json.JSONDecodeError as e:
                print(f"⚠️ Skipping malformed line {line_num}: {e}")
                continue
    
    # Write migrated data back
    temp_path = memory_file.with_suffix('.tmp')
    
    try:
        with open(temp_path, 'w') as f:
            # Write entities
            for entity in entities:
                f.write(json.dumps(entity, separators=(',', ':')) + '\n')
            
            # Write relations  
            for relation in relations:
                f.write(json.dumps(relation, separators=(',', ':')) + '\n')
        
        # Atomic move
        shutil.move(str(temp_path), str(memory_file))
        
        print(f"✅ Migration complete!")
        print(f"   📊 Migrated {migration_count} string observations to JSON objects")
        print(f"   📁 Updated file: {memory_file}")
        
        return True
        
    except Exception as e:
        print(f"❌ Migration failed: {e}")
        # Clean up temp file
        if temp_path.exists():
            temp_path.unlink()
        return False


def _infer_tags_from_content(content: str) -> List[str]:
    """
    Infer appropriate tags based on observation content.
    This is a simple heuristic-based approach.
    """
    content_lower = content.lower()
    tags = []
    
    # Technical indicators
    if any(term in content_lower for term in ['implemented', 'code', 'function', 'api', 'database', 'algorithm']):
        tags.append('technical')
    
    # Resolution indicators
    if any(term in content_lower for term in ['completed', 'resolved', 'fixed', 'done', 'successful']):
        tags.append('resolved')
    
    # Work in progress indicators
    if any(term in content_lower for term in ['working on', 'in progress', 'developing', 'building']):
        tags.append('in-progress')
    
    # Discovery/Learning indicators
    if any(term in content_lower for term in ['discovered', 'learned', 'found', 'identified', 'analysis']):
        tags.append('discovery')
    
    # Architecture/Design indicators
    if any(term in content_lower for term in ['architecture', 'design', 'pattern', 'structure', 'system']):
        tags.append('architecture')
    
    # Documentation indicators
    if any(term in content_lower for term in ['documentation', 'guide', 'tutorial', 'reference', 'manual']):
        tags.append('documentation')
    
    # Problem/Issue indicators
    if any(term in content_lower for term in ['problem', 'issue', 'bug', 'error', 'failure']):
        tags.append('issue')
    
    # Configuration indicators
    if any(term in content_lower for term in ['config', 'setting', 'environment', 'setup']):
        tags.append('configuration')
    
    # Session/Workflow indicators
    if any(term in content_lower for term in ['session', 'workflow', 'process', 'method']):
        tags.append('workflow')
    
    return tags


def main():
    parser = argparse.ArgumentParser(
        description='Migrate string-based observations to JSON objects with timestamps',
        epilog='''
Examples:
  # Migrate with backup (recommended)
  %(prog)s fl-memory.json
  
  # Migrate without backup
  %(prog)s fl-memory.json --no-backup
  
  # Migrate specific file with custom path
  %(prog)s /path/to/memory.json

The migration process:
1. Creates a timestamped backup (unless --no-backup)
2. Converts string observations to JSON objects with:
   - Retroactive timestamps (spread over past week)
   - Original content preserved
   - Auto-inferred tags based on content analysis
3. Preserves existing JSON object observations unchanged
4. Maintains all entities and relations
        ''',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument('memory_file', help='Path to fl-memory.json file to migrate')
    parser.add_argument('--no-backup', action='store_true', 
                       help='Skip creating backup before migration')
    
    args = parser.parse_args()
    
    success = migrate_memory_file(args.memory_file, backup=not args.no_backup)
    
    if success:
        print("\n🎉 Migration completed successfully!")
        print("📝 All string observations have been converted to JSON objects")
        print("🔍 Use 'python log_observation.py get-entity <name>' to see the new format")
    else:
        print("\n💥 Migration failed!")
        exit(1)


if __name__ == "__main__":
    main()