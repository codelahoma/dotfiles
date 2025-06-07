#!/usr/bin/env python3
"""
FlowLoom Installer Bundler Manager

Validates and updates the installer bundler with current project files.
Ensures installer templates stay in sync with the main project.
"""

import os
import sys
import json
import shutil
import argparse
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Tuple, Optional


class InstallerBundler:
    def __init__(self, project_root: Path = None):
        self.project_root = project_root or Path(__file__).parent.parent
        self.installer_resources = self.project_root / "packages" / "flowloom_installer" / "src" / "flowloom_installer" / "resources"
        self.sync_mappings = self._get_sync_mappings()
        
    def _get_sync_mappings(self) -> List[Dict]:
        """Get mappings of source files to installer resource destinations."""
        return [
            {
                "name": "FlowLoom Commands",
                "source": self.project_root / ".claude",
                "dest": self.installer_resources / ".claude",
                "type": "directory",
                "required": True
            },
            {
                "name": "CLAUDE.md Template", 
                "source": self.project_root / "CLAUDE.md",
                "dest": self.installer_resources / "CLAUDE.md",
                "type": "file",
                "required": True
            },
            {
                "name": "CLAUDE.local.md Template",
                "source": self.project_root / "CLAUDE.local.md", 
                "dest": self.installer_resources / "CLAUDE.local.md",
                "type": "file",
                "required": True
            },
            {
                "name": "MCP Configuration",
                "source": self.project_root / ".mcp.json",
                "dest": self.installer_resources / ".mcp.json", 
                "type": "file",
                "required": False
            },
            {
                "name": "Settings Template",
                "source": self.project_root / "settings.local.json",
                "dest": self.installer_resources / "settings.local.json",
                "type": "file", 
                "required": False
            },
            {
                "name": "WORM Source Code",
                "source": self.project_root / "src" / "worm",
                "dest": self.installer_resources / "worm",
                "type": "directory",
                "required": True
            }
        ]
    
    def validate(self) -> Tuple[bool, List[str]]:
        """Validate installer bundle against current project state."""
        issues = []
        
        # Check if installer resources directory exists
        if not self.installer_resources.exists():
            issues.append(f"❌ Installer resources directory not found: {self.installer_resources}")
            return False, issues
            
        print(f"🔍 Validating installer bundle...")
        print(f"Project root: {self.project_root}")
        print(f"Installer resources: {self.installer_resources}")
        print()
        
        for mapping in self.sync_mappings:
            source = mapping["source"]
            dest = mapping["dest"] 
            name = mapping["name"]
            required = mapping["required"]
            
            # Check source exists
            if not source.exists():
                if required:
                    issues.append(f"❌ Required source missing: {name} ({source})")
                else:
                    print(f"⚠️  Optional source missing: {name}")
                continue
                
            # Check destination exists
            if not dest.exists():
                issues.append(f"❌ Missing from installer: {name} ({dest})")
                continue
                
            # Compare content (basic check)
            if mapping["type"] == "file":
                if self._files_different(source, dest):
                    issues.append(f"⚠️  Out of sync: {name}")
                else:
                    print(f"✅ In sync: {name}")
            else:
                # Directory comparison (simplified)
                source_files = self._count_files(source)
                dest_files = self._count_files(dest)
                if source_files != dest_files:
                    issues.append(f"⚠️  File count mismatch: {name} (source: {source_files}, dest: {dest_files})")
                else:
                    print(f"✅ In sync: {name} ({source_files} files)")
        
        print()
        if not issues:
            print("🎉 Installer bundle is valid and up to date!")
            return True, []
        else:
            print("❌ Installer bundle validation issues found:")
            for issue in issues:
                print(f"   {issue}")
            return False, issues
    
    def update(self, dry_run: bool = False) -> bool:
        """Update installer bundle with current project files."""
        mode = "DRY RUN" if dry_run else "LIVE UPDATE"
        print(f"🔄 Installer Bundle Update ({mode})")
        print("=" * 50)
        print(f"Project root: {self.project_root}")
        print(f"Installer resources: {self.installer_resources}")
        print()
        
        if not dry_run and not self.installer_resources.exists():
            self.installer_resources.mkdir(parents=True, exist_ok=True)
            print(f"📁 Created installer resources directory")
            
        updated = 0
        skipped = 0
        
        for mapping in self.sync_mappings:
            source = mapping["source"]
            dest = mapping["dest"]
            name = mapping["name"]
            required = mapping["required"]
            
            if not source.exists():
                if required:
                    print(f"❌ SKIP: {name} (required source missing)")
                else:
                    print(f"⚠️  SKIP: {name} (optional source missing)")
                skipped += 1
                continue
                
            if dry_run:
                print(f"📋 WOULD UPDATE: {name}")
                print(f"   FROM: {source}")
                print(f"   TO:   {dest}")
                if mapping["type"] == "directory":
                    file_count = self._count_files(source)
                    print(f"   FILES: {file_count}")
                else:
                    size = source.stat().st_size if source.exists() else 0
                    print(f"   SIZE: {size} bytes")
            else:
                print(f"🔄 UPDATING: {name}")
                
                # Create parent directory
                dest.parent.mkdir(parents=True, exist_ok=True)
                
                # Remove existing destination
                if dest.exists():
                    if dest.is_dir():
                        shutil.rmtree(dest)
                    else:
                        dest.unlink()
                
                # Copy source to destination
                if mapping["type"] == "directory":
                    shutil.copytree(source, dest)
                    file_count = self._count_files(dest)
                    print(f"   ✅ Copied directory ({file_count} files)")
                else:
                    shutil.copy2(source, dest)
                    size = dest.stat().st_size
                    print(f"   ✅ Copied file ({size} bytes)")
                    
                updated += 1
            print()
        
        # Summary
        print("📊 Update Summary")
        print("=" * 20)
        if dry_run:
            print("This was a dry run. Use --update to perform actual changes.")
        else:
            print(f"✅ Updated {updated} resources")
            if skipped > 0:
                print(f"⚠️  Skipped {skipped} resources")
            print()
            print("🔧 Next steps:")
            print("1. Test installer with updated resources")
            print("2. Run validation to confirm sync")
            print("3. Consider updating installer version")
        
        return True
    
    def _files_different(self, file1: Path, file2: Path) -> bool:
        """Simple file comparison (could be enhanced)."""
        try:
            return file1.read_text() != file2.read_text()
        except Exception:
            return True
            
    def _count_files(self, directory: Path) -> int:
        """Count files in directory recursively.""" 
        try:
            return len(list(directory.rglob("*"))) if directory.is_dir() else 0
        except Exception:
            return 0


def main():
    parser = argparse.ArgumentParser(
        description="FlowLoom Installer Bundler Manager",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s --validate                  # Check if installer is up to date
  %(prog)s --update --dry-run         # Show what would be updated  
  %(prog)s --update                   # Update installer with current project files
  
This tool ensures the installer bundle stays in sync with the main project.
        """
    )
    
    parser.add_argument("--validate", action="store_true",
                       help="Validate installer bundle against project")
    parser.add_argument("--update", action="store_true", 
                       help="Update installer bundle with current project files")
    parser.add_argument("--dry-run", action="store_true",
                       help="Show what would be done without making changes")
    parser.add_argument("--project-root", type=Path,
                       help="Override project root directory")
    
    args = parser.parse_args()
    
    if not args.validate and not args.update:
        parser.error("Must specify either --validate or --update")
        
    if args.dry_run and not args.update:
        parser.error("--dry-run can only be used with --update")
    
    bundler = InstallerBundler(args.project_root)
    
    try:
        if args.validate:
            success, issues = bundler.validate()
            sys.exit(0 if success else 1)
            
        if args.update:
            success = bundler.update(args.dry_run)
            sys.exit(0 if success else 1)
            
    except Exception as e:
        print(f"❌ Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()