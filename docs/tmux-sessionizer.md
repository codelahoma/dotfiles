# tmux-sessionizer

A fuzzy-finding tmux session manager with frecency sorting, project detection, git status indicators, rich previews, and automatic session templates.

Inspired by [ThePrimeagen's tmux-sessionizer](https://github.com/ThePrimeagen/.dotfiles), extended with a Rust-based scanner for performance and a suite of quality-of-life features.

---

## Table of Contents

- [Quick Start](#quick-start)
- [Architecture](#architecture)
- [Installation](#installation)
- [Configuration](#configuration)
- [Features](#features)
  - [Fuzzy Finding with fzf](#fuzzy-finding-with-fzf)
  - [Frecency Sorting](#frecency-sorting)
  - [Project Type Detection](#project-type-detection)
  - [Git Status Indicators](#git-status-indicators)
  - [Rich Preview Pane](#rich-preview-pane)
  - [Session Templates](#session-templates)
  - [Home Top-Level Inclusion](#home-top-level-inclusion)
- [Keyboard Shortcuts](#keyboard-shortcuts)
- [Rust Scanner (`tmux-sessionizer-scan`)](#rust-scanner-tmux-sessionizer-scan)
- [Shell Fallback](#shell-fallback)
- [Usage Statistics](#usage-statistics)
- [File Reference](#file-reference)
- [Troubleshooting](#troubleshooting)
- [Dependencies](#dependencies)

---

## Quick Start

```bash
# From anywhere in the shell
Ctrl-F

# From inside tmux (prefix + f)
Ctrl-A f

# Or run directly
~/bin/tmux-sessionizer
```

This opens an fzf picker listing your project directories. Select one to create or switch to a named tmux session rooted in that directory.

---

## Architecture

```
tmux-sessionizer (zsh script)
├── Reads config from ~/.config/tmux-sessionizer/config
├── Delegates directory scanning to:
│   ├── tmux-sessionizer-scan (Rust binary, preferred)
│   └── Shell fallback (built-in, slower)
├── Pipes formatted list into fzf
├── On selection:
│   ├── Tracks usage stats (if enabled)
│   ├── Creates tmux session (if new)
│   ├── Applies session template (if enabled)
│   └── Attaches or switches to session
└── Preview mode (--preview flag, called by fzf)
```

The system has two main components:

1. **`~/bin/tmux-sessionizer`** - The main zsh script that handles configuration loading, fzf integration, session management, preview rendering, and template application.

2. **`~/bin/tmux-sessionizer-scan`** - A compiled Rust binary that handles the performance-critical work: directory scanning, git status checks (parallelized with libgit2), frecency computation, and stats tracking.

---

## Installation

The script is part of the dotfiles repo and gets symlinked via homeshick:

```
dotfiles repo                          → symlinked to ~
─────────────────────────────────────────────────────────
home/bin/tmux-sessionizer              → ~/bin/tmux-sessionizer
home/bin/tmux-sessionizer-scan         → ~/bin/tmux-sessionizer-scan
home/.config/tmux-sessionizer/config   → ~/.config/tmux-sessionizer/config
home/.config/tmux-sessionizer/templates/* → ~/.config/tmux-sessionizer/templates/*
```

### Building the Rust Scanner

```bash
cd tools/tmux-sessionizer-scan

# Build and install to home/bin/
make install

# Or build only
make build
# Binary is at target/release/tmux-sessionizer-scan
```

The Rust scanner is optional. If the binary is missing or fails, the script falls back to a pure-shell implementation automatically.

---

## Configuration

**Config file:** `~/.config/tmux-sessionizer/config`

This file is sourced as a zsh script on startup. All variables have defaults in the main script.

```bash
# Directories to scan for projects.
# Each path is included as-is, plus its children up to MAX_DEPTH.
SEARCH_PATHS=(
  "$HOME"
  "$HOME/github"
  "$HOME/gitlab"
  "$HOME/personal"
  "$HOME/personal/planning"
  "$HOME/work"
  "$HOME/.homesick/repos/dotfiles"
  "$HOME/Dropbox/org"          # Pinned: included as-is, not children
)

# How many levels deep to scan inside each search path.
# 1 = immediate subdirectories only.
MAX_DEPTH=1

# Feature toggles (all default to false if not set)
ENABLE_PREVIEW=true        # Rich fzf preview pane
ENABLE_STATS=true          # Track usage frequency and recency
ENABLE_GIT_STATUS=true     # Show git status indicators
ENABLE_TEMPLATES=true      # Apply session templates on creation
INCLUDE_HOME_TOPLEVEL=true # Include ~/*/dirs not already in SEARCH_PATHS

# File locations
STATS_FILE="$HOME/.config/tmux-sessionizer/stats.json"
TEMPLATE_DIR="$HOME/.config/tmux-sessionizer/templates"
```

### Configuration Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `SEARCH_PATHS` | `~/personal`, `~/work`, `~/github`, `~/gitlab`, `~/.homesick/repos/dotfiles/home` | Array of directories to scan |
| `MAX_DEPTH` | `1` | Subdirectory depth within each search path |
| `ENABLE_PREVIEW` | `false` | Show rich preview pane in fzf |
| `ENABLE_STATS` | `false` | Track and sort by frecency |
| `ENABLE_GIT_STATUS` | `false` | Show git repo status indicators |
| `ENABLE_TEMPLATES` | `false` | Apply layout templates on session creation |
| `INCLUDE_HOME_TOPLEVEL` | `false` | Add `~/*/` directories not covered by SEARCH_PATHS |
| `STATS_FILE` | `~/.config/tmux-sessionizer/stats.json` | Path to usage statistics JSON |
| `TEMPLATE_DIR` | `~/.config/tmux-sessionizer/templates` | Directory containing template scripts |

---

## Features

### Fuzzy Finding with fzf

The directory list is piped through fzf with ANSI color support. The `--tiebreak=index` option preserves frecency ordering when fzf match scores are equal, so your most-used projects stay at the top even as you type.

### Frecency Sorting

When `ENABLE_STATS=true`, projects are ranked by a **frecency** score combining frequency of use and recency of access:

```
frecency = count * 0.5 + recency_weight * 50 * 0.5
```

Recency weights:

| Time since last use | Weight |
|---------------------|--------|
| < 1 day             | 1.0    |
| 1-7 days            | 0.8    |
| 7-30 days           | 0.5    |
| > 30 days           | 0.2    |

Projects with a frecency score > 20 receive a star indicator.

**Example display:**
```
⭐ 98    ✓ 🏠 /Users/rodk/.homesick/repos/dotfiles  (2h ago, 42x)
⭐ 87    ✗ 🐍 /Users/rodk/projects/my-app           (1d ago, 28x)
   12      📦 /Users/rodk/old-project               (30d ago, 5x)
    0      💻 /Users/rodk/new-project
```

### Project Type Detection

Directories are classified by marker files:

| Type | Detection Markers | Icon |
|------|-------------------|------|
| Rust | `Cargo.toml` | `🦀` |
| Node.js | `package.json` | `📦` |
| Python | `pyproject.toml`, `setup.py`, or `requirements.txt` | `🐍` |
| Go | `go.mod` | `🐹` |
| Dotfiles | `.homesick/` dir or `"dotfiles"` in git remote URL | `🏠` |
| Ruby | `Gemfile` | `💎` |
| Java | `pom.xml` or `build.gradle` | `☕` |
| General | (fallback) | `💻` |

Detection is checked in the order listed; first match wins.

### Git Status Indicators

When `ENABLE_GIT_STATUS=true`, each git repository shows its state:

| Indicator | Meaning |
|-----------|---------|
| `✓` | Clean, in sync with upstream |
| `✗` | Uncommitted changes (dirty working tree) |
| `~` | Ahead of or behind upstream |
| ` ` (space) | Not a git repository |

The Rust scanner checks git status using libgit2 with bounded parallelism (up to 8 threads), making it significantly faster than shell-based `git status` calls.

### Rich Preview Pane

When `ENABLE_PREVIEW=true`, fzf shows a preview panel (toggled with `Ctrl-/`) containing:

```
📂 Project: dotfiles
📍 /Users/rodk/.homesick/repos/dotfiles

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

The preview is generated by re-invoking the script with `--preview`, which runs git commands and file listings against the selected directory.

### Session Templates

When `ENABLE_TEMPLATES=true`, newly created sessions automatically get a window/pane layout based on the project type. Templates are bash scripts that receive the session name as `$1`.

**Template resolution order:**
1. `.tmux-template` in the project root (per-project override)
2. `$TEMPLATE_DIR/<project_type>.tmux` (type-specific)
3. `$TEMPLATE_DIR/default.tmux` (fallback)

#### Built-in Templates

**`python.tmux`** - Editor + virtualenv terminal
```
┌──────────────────┬─────────┐
│                  │ terminal│
│   editor (70%)   │  (30%)  │
│                  │ + venv  │
└──────────────────┴─────────┘
```
Auto-activates `.venv` or `venv` virtualenv in the right pane.

**`rust.tmux`** - Editor + cargo terminal
```
┌──────────────────┬─────────┐
│                  │  cargo  │
│   editor (70%)   │  hints  │
│                  │  (30%)  │
└──────────────────┴─────────┘
```
Shows cargo command hints (build, run, test, check).

**`nodejs.tmux`** - Editor + npm terminal
```
┌──────────────────┬─────────┐
│                  │   npm   │
│   editor (70%)   │  hints  │
│                  │  (30%)  │
└──────────────────┴─────────┘
```
Shows `npm run <tab>` hint.

**`go.tmux`** - Editor + go terminal
```
┌──────────────────┬─────────┐
│                  │   go    │
│   editor (70%)   │  hints  │
│                  │  (30%)  │
└──────────────────┴─────────┘
```
Shows go command hints (build, run, test).

**`dotfiles.tmux`** - Editor + shell + git watch
```
┌──────────────────────────────┐
│                              │
│      editor (60%)            │
│                              │
├──────────────┬───────────────┤
│    shell     │  git status   │
│              │   (watch)     │
└──────────────┴───────────────┘
```
Three panes: editor on top, shell bottom-left, auto-refreshing `git status` bottom-right.

**`general.tmux`** - Simple editor + terminal
```
┌──────────────────┬─────────┐
│                  │         │
│   editor (70%)   │ terminal│
│                  │  (30%)  │
└──────────────────┴─────────┘
```

**`default.tmux`** - Single pane, no splits.

#### Custom Per-Project Templates

Create a `.tmux-template` file in any project root:

```bash
#!/usr/bin/env bash
SESSION="$1"

# Example: 3-pane layout with dev server
tmux split-window -h -t "$SESSION:1" -p 30
tmux split-window -v -t "$SESSION:1.2"
tmux send-keys -t "$SESSION:1.2" 'npm run dev' C-m
tmux send-keys -t "$SESSION:1.3" 'npm run test -- --watch' C-m
tmux select-pane -t "$SESSION:1.1"
```

Make it executable: `chmod +x .tmux-template`

### Home Top-Level Inclusion

When `INCLUDE_HOME_TOPLEVEL=true`, all directories directly under `~/` are included in the list, except:
- Hidden directories (starting with `.`)
- Directories already covered by `SEARCH_PATHS` (or children thereof)

This ensures you can quickly jump to any top-level home directory without explicitly adding every one to `SEARCH_PATHS`.

---

## Keyboard Shortcuts

### Shell

| Key | Action |
|-----|--------|
| `Ctrl-F` | Launch tmux-sessionizer (bound in `.zshrc`) |

### Inside tmux

| Key | Action |
|-----|--------|
| `Ctrl-A f` | Open tmux-sessionizer in a new tmux window (bound in `.tmux.conf.local`) |

### Inside fzf

| Key | Action |
|-----|--------|
| `Enter` | Select directory and switch to session |
| `Ctrl-/` | Toggle preview pane (when `ENABLE_PREVIEW=true`) |
| `Ctrl-C` / `Esc` | Cancel |
| Type characters | Fuzzy filter the list |

---

## Rust Scanner (`tmux-sessionizer-scan`)

The Rust scanner is a performance-optimized companion binary that replaces the shell-based directory scanning and formatting. It provides:

- Fast directory traversal via `std::fs`
- Parallel git status checks using `libgit2` (up to 8 worker threads)
- Native frecency computation and sorting
- JSON stats tracking (`--track` mode)

### Source Location

```
tools/tmux-sessionizer-scan/
├── Cargo.toml       # Dependencies: serde, serde_json, git2
├── Makefile          # build / install / clean targets
└── src/main.rs       # Single-file implementation (~470 lines)
```

### CLI Usage

The scanner is called by the main script and not typically invoked directly:

```bash
# Scan mode (default) — outputs formatted directory list
tmux-sessionizer-scan \
  --search-paths ~/github ~/work ~/personal \
  --max-depth 1 \
  --enable-stats --stats-file ~/.config/tmux-sessionizer/stats.json \
  --enable-git-status \
  --include-home-toplevel

# Track mode — record a session launch in stats
tmux-sessionizer-scan --track /path/to/project \
  --stats-file ~/.config/tmux-sessionizer/stats.json
```

### Scanner CLI Flags

| Flag | Description |
|------|-------------|
| `--search-paths <path>...` | Directories to scan (consumes all args until next `--`) |
| `--max-depth <n>` | Subdirectory search depth (default: 1) |
| `--enable-stats` | Include frecency scores in output |
| `--stats-file <path>` | Path to stats JSON file |
| `--enable-git-status` | Include git status indicators |
| `--include-home-toplevel` | Add `~/*/` directories |
| `--track <path>` | Track a session launch (increment count, update timestamp) |

### Building

```bash
cd tools/tmux-sessionizer-scan

# Build optimized release binary (LTO + stripped)
make build

# Build and copy to home/bin/
make install

# Clean build artifacts
make clean
```

Build profile uses `opt-level = 3`, link-time optimization (`lto = true`), and symbol stripping (`strip = true`) for a small, fast binary.

---

## Shell Fallback

If the Rust scanner binary is not found or fails, the script falls back to a pure-shell implementation. This uses:

- `find` for directory enumeration
- `git` CLI for status checks (sequential, not parallel)
- `jq` for stats JSON manipulation

A warning is printed to stderr: `"Warning: tmux-sessionizer-scan not available, using slow shell fallback"`

The fallback produces identical output format and supports all the same features, just slower.

---

## Usage Statistics

Stats are stored as JSON at `~/.config/tmux-sessionizer/stats.json`:

```json
{
  "dotfiles": {
    "path": "/Users/rodk/.homesick/repos/dotfiles",
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

Each entry is keyed by the directory's `basename`. Fields:

| Field | Type | Description |
|-------|------|-------------|
| `path` | string | Full directory path |
| `count` | integer | Total number of times the session was launched |
| `lastUsed` | integer | Unix timestamp of last launch |

### Managing Stats

```bash
# Reset all statistics
echo '{}' > ~/.config/tmux-sessionizer/stats.json

# View current stats (pretty-printed)
jq . ~/.config/tmux-sessionizer/stats.json

# Remove a single entry
jq 'del(.["old-project"])' ~/.config/tmux-sessionizer/stats.json > /tmp/stats.json \
  && mv /tmp/stats.json ~/.config/tmux-sessionizer/stats.json
```

---

## File Reference

| File | Purpose |
|------|---------|
| `home/bin/tmux-sessionizer` | Main script (zsh) |
| `home/bin/tmux-sessionizer-scan` | Compiled Rust scanner binary |
| `home/.config/tmux-sessionizer/config` | Configuration file (sourced as zsh) |
| `home/.config/tmux-sessionizer/stats.json` | Usage statistics (generated at runtime) |
| `home/.config/tmux-sessionizer/templates/python.tmux` | Python project layout |
| `home/.config/tmux-sessionizer/templates/rust.tmux` | Rust project layout |
| `home/.config/tmux-sessionizer/templates/nodejs.tmux` | Node.js project layout |
| `home/.config/tmux-sessionizer/templates/go.tmux` | Go project layout |
| `home/.config/tmux-sessionizer/templates/dotfiles.tmux` | Dotfiles project layout |
| `home/.config/tmux-sessionizer/templates/general.tmux` | Generic project layout |
| `home/.config/tmux-sessionizer/templates/default.tmux` | Default (single pane, no splits) |
| `tools/tmux-sessionizer-scan/` | Rust scanner source and build files |
| `home/.zshrc` (line 52) | `Ctrl-F` keybinding |
| `home/.tmux.conf.local` (line 355) | `prefix + f` keybinding |

---

## Troubleshooting

### Preview pane not showing
- Verify `ENABLE_PREVIEW=true` in config
- Check the script is executable: `ls -l ~/bin/tmux-sessionizer`
- Try running preview manually: `~/bin/tmux-sessionizer --preview "test\t$HOME"`

### Templates not applying
- Verify `ENABLE_TEMPLATES=true` in config
- Templates only apply to **newly created** sessions, not existing ones
- Check template files are executable: `ls -l ~/.config/tmux-sessionizer/templates/`

### Git status not showing
- Verify `ENABLE_GIT_STATUS=true` in config
- Only git repositories show status; non-git directories show a space

### Statistics not tracking
- Verify `ENABLE_STATS=true` in config
- Check the stats file is writable: `touch ~/.config/tmux-sessionizer/stats.json`
- The shell fallback requires `jq`: `brew install jq`

### Scanner not found / using shell fallback
- Rebuild: `cd tools/tmux-sessionizer-scan && make install`
- Check binary exists: `ls -l ~/bin/tmux-sessionizer-scan`
- Verify it runs: `~/bin/tmux-sessionizer-scan --search-paths /dev/null`

### Session name conflicts
Session names are derived from `basename` of the directory path, with `.` replaced by `_`. If two directories share the same basename (e.g., `~/work/api` and `~/personal/api`), they will map to the same session name and the second selection will switch to the existing session rather than creating a new one.

---

## Dependencies

| Dependency | Required | Purpose |
|------------|----------|---------|
| `tmux` | Yes | Session management |
| `fzf` | Yes | Fuzzy finding UI |
| `zsh` | Yes | Script interpreter |
| `git` | No | Git status indicators and preview info |
| `jq` | No | Stats tracking in shell fallback |
| `libgit2` | No | Linked into Rust scanner for fast git operations |
