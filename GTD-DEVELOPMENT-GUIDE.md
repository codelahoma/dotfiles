# GTD System Development Guide

## Development Workflow

### 1. Making Changes
When modifying the GTD system:

1. Edit files in `home/.spacemacs.d/codelahoma-gtd/`
2. Use `SPC o o d r` to reload without restarting Spacemacs
3. Test your changes immediately
4. Use `SPC o o d v` to validate structure after changes

### 2. Testing Commands

#### Reload System
- **Command**: `SPC o o d r` (`codelahoma-gtd-reload`)
- **Purpose**: Reload all GTD modules without restarting Spacemacs
- **Usage**: After editing any GTD elisp files

#### Benchmark Capture
- **Command**: `SPC o o d b` (`codelahoma-gtd-benchmark-capture`)
- **Purpose**: Test capture performance
- **Target**: < 1.0 second for capture operations
- **Output**: Time in seconds shown in minibuffer

#### Validate Structure
- **Command**: `SPC o o d v` (`codelahoma-gtd-validate-structure`)
- **Purpose**: Check all directories and files exist
- **Output**: Success message or list of missing items

#### Initialize System
- **Command**: `SPC o o d i` (`codelahoma-gtd-initialize`)
- **Purpose**: Create all directories and placeholder files
- **Usage**: Run once during initial setup

### 3. Test Files

The following test files are available:
- `test-gtd-system.el` - Basic system functionality tests
- `test-org-roam.el` - Org-roam configuration tests
- `test-keybindings.el` - Keybinding verification
- `verify-gtd-structure.el` - Comprehensive structure validation

Run tests with:
```elisp
M-x load-file RET test-gtd-system.el RET
```

### 4. Performance Targets

Phase 1 Targets:
- Capture operation: < 1.0 second
- File navigation: < 0.5 seconds
- System reload: < 2.0 seconds

### 5. Module Structure

```
~/.spacemacs.d/
├── codelahoma-gtd/
│   ├── codelahoma-gtd-config.el    # Configuration variables
│   ├── codelahoma-gtd-core.el      # Core functionality
│   ├── codelahoma-gtd-capture.el   # Capture templates
│   ├── codelahoma-gtd-process.el   # Processing workflows
│   ├── codelahoma-gtd-review.el    # Review processes
│   └── codelahoma-gtd-roam.el      # Org-roam integration
├── codelahoma-ui.el                # Keybindings and UI
└── codelahoma-bridge.el            # GTD-Zettelkasten bridge (Phase 5)
```

### 6. Adding New Features

1. Identify which module the feature belongs to
2. Add the function to the appropriate module
3. Add keybinding in `codelahoma-ui.el` if needed
4. Update `codelahoma-gtd-reload` if adding new modules
5. Test with development commands
6. Update documentation

### 7. Debugging Tips

- Use `M-x toggle-debug-on-error` for stack traces
- Check `*Messages*` buffer for output
- Use `(message "Debug: %s" variable)` for debugging
- Validate symlinks with `ls -la ~/.spacemacs.d/codelahoma-gtd/`

### 8. Git Workflow

```bash
# After making changes
git add -A
git commit -m "feat(gtd): Description of changes"
git push

# For fixes
git commit -m "fix(gtd): What was fixed"

# For documentation
git commit -m "docs(gtd): What was documented"
```

### 9. Common Issues

#### Module Not Loading
- Check symlinks exist (run `setup-gtd-complete.sh`)
- Verify `(require 'module-name)` statements
- Check for syntax errors with `M-x check-parens`

#### Keybinding Not Working
- Reload with `SPC o o d r`
- Check for conflicts with `test-keybindings.el`
- Verify function is interactive

#### Performance Issues
- Use `SPC o o d b` to benchmark
- Profile with `M-x profiler-start` / `M-x profiler-report`
- Check for unnecessary file operations