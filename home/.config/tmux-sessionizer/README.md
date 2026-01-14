# tmux-sessionizer

A powerful, intelligent tmux session manager with fuzzy finding, usage tracking, and automatic project layouts.

## Features

### 🎯 Core Features
- **Fuzzy finding** with FZF for quick project selection
- **Smart session management** - create, switch, or attach to tmux sessions
- **Automatic project detection** - recognizes Python, Rust, Node.js, Go, and more
- **Usage statistics** - tracks frequency and recency (frecency algorithm)
- **Rich preview pane** - shows git status, recent files, and project info
- **Session templates** - automatic window/pane layouts per project type
- **Visual indicators** - emoji icons for project types and git status

### 🔥 Smart Session Exit
When you exit the last shell in a tmux session, tmux will automatically switch to your most recently used session instead of detaching. Only exits cleanly when the last session closes.

## Installation

The script is already installed at `~/bin/tmux-sessionizer` and bound to `Ctrl-F` in your shell.

## Quick Start

### Basic Usage
```bash
# Launch with keyboard shortcut
Ctrl-F

# Or run directly
~/bin/tmux-sessionizer
```

### Enable Enhanced Features
Edit `~/.config/tmux-sessionizer/config`:
```bash
ENABLE_PREVIEW=true        # Rich preview pane
ENABLE_STATS=true          # Usage tracking and sorting
ENABLE_GIT_STATUS=true     # Git status indicators
ENABLE_TEMPLATES=true      # Auto-apply project layouts
```

## Configuration

### Config File: `~/.config/tmux-sessionizer/config`

```bash
# Search paths for projects
SEARCH_PATHS=(
  "$HOME"
  "$HOME/personal"
  "$HOME/work"
  "$HOME/github"
  "$HOME/.homesick/repos/dotfiles/home"
)

# Maximum depth for directory search
MAX_DEPTH=1

# Feature toggles
ENABLE_PREVIEW=true
ENABLE_STATS=true
ENABLE_GIT_STATUS=true
ENABLE_TEMPLATES=true

# File locations
STATS_FILE="$HOME/.config/tmux-sessionizer/stats.json"
TEMPLATE_DIR="$HOME/.config/tmux-sessionizer/templates"
```

## Features in Detail

### Frecency Scoring (ENABLE_STATS=true)
Projects are sorted by a combination of:
- **Frequency**: How many times you've accessed the project
- **Recency**: How recently you accessed it

Scoring algorithm:
- Recent use (< 1 day): 1.0 weight
- Last week: 0.8 weight
- Last month: 0.5 weight
- Older: 0.2 weight

Projects with score > 20 get a ⭐ indicator.

**Example display:**
```
⭐ 98  ✓ 🏠 /Users/you/.homesick/repos/dotfiles  (2h ago, 42x)
⭐ 87  ✗ 🐍 /Users/you/projects/my-app          (yesterday, 28x)
   12    📦 /Users/you/old-project              (30d ago, 5x)
```

### Project Type Detection
Automatically detects project types from markers:

| Type | Markers | Icon |
|------|---------|------|
| Rust | Cargo.toml | 🦀 |
| Node.js | package.json | 📦 |
| Python | pyproject.toml, setup.py, requirements.txt | 🐍 |
| Go | go.mod | 🐹 |
| Ruby | Gemfile | 💎 |
| Java | pom.xml, build.gradle | ☕ |
| Dotfiles | .homesick or "dotfiles" in git remote | 🏠 |
| General | Everything else | 💻 |

### Git Status Indicators (ENABLE_GIT_STATUS=true)
Shows repository state at a glance:
- `✓` Clean and in sync
- `✗` Uncommitted changes (dirty)
- `~` Ahead or behind remote
- ` ` Not a git repository

### Preview Pane (ENABLE_PREVIEW=true)
Press `Ctrl-/` in FZF to toggle a rich preview showing:

```
📂 Project: dotfiles
📍 /Users/rodk/.homesick/repos/dotfiles/home
🔥 Used 42 times (last: 2h ago)

Git Status:
  Branch: master
  Status: ✓ Clean
  ↑ 2 commit(s) ahead

Recent Files:
  bin/tmux-sessionizer
  .tmux.conf.local
  .hammerspoon/init.org

Project Type: 🏠 dotfiles
Template: dotfiles.tmux
```

### Session Templates (ENABLE_TEMPLATES=true)
Automatically configures window/pane layouts when creating new sessions.

#### Built-in Templates

**python.tmux** - Python projects
```
┌──────────────────┬─────┐
│                  │     │
│   Editor (70%)   │ CLI │
│                  │(30%)│
│                  │     │
└──────────────────┴─────┘
```
- Auto-activates virtualenv (.venv or venv)
- Editor pane on left, terminal on right

**nodejs.tmux** - Node.js projects
```
┌──────────────────┬─────┐
│                  │     │
│   Editor (70%)   │ CLI │
│                  │(30%)│
└──────────────────┴─────┘
```
- Shows npm scripts hint
- Ready for `npm run` commands

**rust.tmux** - Rust projects
```
┌──────────────────┬─────┐
│                  │cargo│
│   Editor (70%)   │ CLI │
│                  │(30%)│
└──────────────────┴─────┘
```
- Shows cargo commands (build, run, test, check)

**dotfiles.tmux** - Dotfiles repositories
```
┌──────────────────────────┐
│                          │
│   Editor (60%)           │
│                          │
├────────────┬─────────────┤
│   Shell    │ Git Status  │
│   (20%)    │  (watch)    │
└────────────┴─────────────┘
```
- Three panes: editor, shell, auto-updating git status

**general.tmux** - All other projects
```
┌──────────────────┬─────┐
│                  │     │
│   Editor (70%)   │ CLI │
│                  │(30%)│
└──────────────────┴─────┘
```
- Simple 70/30 split

#### Custom Templates
Create `.tmux-template` in your project root:

```bash
#!/usr/bin/env bash
# Custom template for this project
SESSION="$1"

# Your custom tmux commands here
tmux split-window -v -t "$SESSION:1"
tmux send-keys -t "$SESSION:1.2" 'npm run dev' C-m
tmux select-pane -t "$SESSION:1.1"
```

Make it executable: `chmod +x .tmux-template`

Templates receive the session name as `$1` and can use any tmux commands.

## Keyboard Shortcuts

**In shell:**
- `Ctrl-F` - Launch tmux-sessionizer

**In FZF:**
- `Ctrl-/` - Toggle preview pane
- `Enter` - Select and switch to session
- `Ctrl-C` / `Esc` - Cancel

**In tmux:**
- `Ctrl-A f` - Open tmux-sessionizer in new window (if configured)
- `exit` in last shell → Switch to previous session (not detach!)

## Usage Statistics

Stats are stored in `~/.config/tmux-sessionizer/stats.json`:

```json
{
  "dotfiles": {
    "path": "/Users/rodk/.homesick/repos/dotfiles/home",
    "count": 42,
    "lastUsed": 1704931200
  },
  "my-project": {
    "path": "/Users/rodk/projects/my-project",
    "count": 28,
    "lastUsed": 1704844800
  }
}
```

To reset statistics:
```bash
echo '{}' > ~/.config/tmux-sessionizer/stats.json
```

## Troubleshooting

### Preview pane not showing
- Ensure `ENABLE_PREVIEW=true` in config
- Check that the script has execute permissions: `ls -l ~/bin/tmux-sessionizer`

### Templates not applying
- Ensure `ENABLE_TEMPLATES=true` in config
- Check template files are executable: `ls -l ~/.config/tmux-sessionizer/templates/`
- Templates only apply to newly created sessions, not existing ones

### Git status not showing
- Ensure `ENABLE_GIT_STATUS=true` in config
- Requires directories to be git repositories

### Statistics not tracking
- Ensure `ENABLE_STATS=true` in config
- Check stats file is writable: `ls -l ~/.config/tmux-sessionizer/stats.json`
- Requires `jq` to be installed: `brew install jq`

## Dependencies

All dependencies should already be installed:
- `tmux` - Session management
- `fzf` - Fuzzy finding
- `git` - Repository status (optional)
- `jq` - JSON parsing for stats (optional)

## Advanced Configuration

### Add More Search Paths
Edit `~/.config/tmux-sessionizer/config`:
```bash
SEARCH_PATHS=(
  "$HOME"
  "$HOME/personal"
  "$HOME/work"
  "$HOME/github"
  "$HOME/projects/client-work"  # Add new path
)
```

### Change Search Depth
Search nested directories:
```bash
MAX_DEPTH=2  # Search 2 levels deep
```

### Disable Specific Features
Turn off features you don't need:
```bash
ENABLE_PREVIEW=false       # No preview pane
ENABLE_STATS=false         # No usage tracking
ENABLE_GIT_STATUS=false    # No git indicators
ENABLE_TEMPLATES=false     # No auto-layouts
```

## Tips & Tricks

### Quickly Access Frequently Used Projects
With stats enabled, your most-used projects automatically rise to the top. Just hit `Ctrl-F` + `Enter` to jump to your main project.

### Create Project-Specific Layouts
Add a `.tmux-template` file to any project for custom layouts:
```bash
cd ~/my-special-project
cat > .tmux-template << 'EOF'
#!/usr/bin/env bash
SESSION="$1"
# 4-pane layout for this specific project
tmux split-window -h -t "$SESSION:1"
tmux split-window -v -t "$SESSION:1.1"
tmux split-window -v -t "$SESSION:1.2"
tmux select-pane -t "$SESSION:1.1"
EOF
chmod +x .tmux-template
```

### Bind to Different Key
Edit `~/.zshrc`:
```bash
# Change from Ctrl-F to Ctrl-P
bindkey -s ^P "tmux-sessionizer\n"
```

## What's New

### Version 2.0 (2026-01)
- ✨ Added frecency-based sorting
- ✨ Project type detection with icons
- ✨ Git status indicators
- ✨ Rich preview pane with FZF
- ✨ Session template system
- ✨ Usage statistics tracking
- ✨ Smart session exit behavior (no more detaching!)
- 📦 Configuration file system
- 🏗️ Complete rewrite in zsh

### Version 1.0 (Original)
- Basic FZF-based session switching
- Hardcoded search paths
- Simple session creation

## License

Part of rodk's dotfiles. Feel free to use and modify for your own dotfiles.

## Credits

Inspired by:
- ThePrimeagen's tmux-sessionizer
- fzf by Junegunn Choi
- gpakosz/.tmux configuration framework
