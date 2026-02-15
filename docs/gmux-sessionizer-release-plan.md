# GMux Sessionizer — Open Source Release Plan

## 1. Background & IP Assessment

### Origin
The original concept of a tmux session fuzzy-finder comes from ThePrimeagen's
`tmux-sessionizer` — a ~40-line bash script that uses `find` + `fzf` to pick a
directory and create/switch tmux sessions. Our README credits this:
"Inspired by: ThePrimeagen's tmux-sessionizer."

### License Situation
- ThePrimeagen's `.dotfiles` and standalone `tmux-sessionizer` repos have **no
  license** (all rights reserved by default).
- An open issue (`.dotfiles#67`) requesting a license has gone unanswered since
  Oct 2024.

### Why We're Clear to Open-Source
This is a **clean-room reimplementation**, not a derivative work:

| Aspect | ThePrimeagen's | Ours |
|--------|---------------|------|
| Language | ~40 lines bash | 381 lines zsh + 472 lines Rust |
| Shared code | — | Zero lines in common |
| Architecture | Single script: find → fzf → tmux | Two-component: zsh orchestrator + Rust scanner |
| Features | Basic find+fzf+session | Frecency scoring, project type detection, git status indicators, rich preview, session templates, usage statistics, config system |

The concept "use fzf to select a directory and create a tmux session" is a
**functional idea** and not copyrightable. Only specific expression (code) is
protectable, and we share none. Many other projects (jrmoulton/tmux-sessionizer,
polarmutex/tmux-sessionizer, various Go ports) have done the same.

**Decision**: Open-source under **MIT License** with continued "Inspired by"
credit to ThePrimeagen.

---

## 2. Project Identity

- **Name**: `gmux-sessionizer`
- **Tagline**: "A fast, intelligent tmux session manager with frecency sorting,
  project detection, and automatic layouts."
- **GitHub org/user**: `codelahoma` (TBD)
- **Repo**: `codelahoma/gmux-sessionizer`

### Open Questions
- [ ] Confirm project name (`gmux-sessionizer` vs `gmux` vs other)
- [ ] Confirm license (MIT vs Apache 2.0 vs dual MIT/Apache 2.0)
- [ ] Confirm GitHub org/user

---

## 3. Proposed Repository Structure

```
gmux-sessionizer/
├── LICENSE                      # MIT License
├── README.md                    # Project README (rewritten for public)
├── CHANGELOG.md                 # Release history
├── CONTRIBUTING.md              # Contribution guidelines
├── Makefile                     # Top-level build/install
│
├── bin/
│   └── gmux-sessionizer        # Main zsh script (renamed from tmux-sessionizer)
│
├── scanner/                     # Rust scanner binary
│   ├── Cargo.toml
│   ├── Cargo.lock
│   └── src/
│       └── main.rs
│
├── config/
│   ├── config.example           # Example config file
│   └── templates/               # Session template files
│       ├── default.tmux
│       ├── general.tmux
│       ├── rust.tmux
│       ├── python.tmux
│       ├── nodejs.tmux
│       ├── go.tmux
│       └── dotfiles.tmux
│
├── docs/
│   ├── configuration.md         # Detailed config reference
│   ├── templates.md             # Template authoring guide
│   └── frecency.md              # Algorithm explanation
│
├── install.sh                   # Installer script
│
└── completions/                 # Shell completions (future)
    └── _gmux-sessionizer        # Zsh completion (future)
```

---

## 4. Generalization Work Required

These changes transform a personal dotfiles tool into a portable, installable
open-source project.

### 4.1 Script Changes (`bin/gmux-sessionizer`)

| Item | Current State | Target State |
|------|--------------|--------------|
| Shebang | `#!/usr/bin/env zsh` | Keep — document zsh requirement |
| Default search paths | Hardcoded personal paths (`$HOME/github`, `$HOME/gitlab`, etc.) | Sensible defaults: `$HOME/projects`, `$HOME/src`, `$HOME/repos` |
| Config file location | `~/.config/tmux-sessionizer/config` | `~/.config/gmux-sessionizer/config` (with `XDG_CONFIG_HOME` support) |
| Stats file location | `~/.config/tmux-sessionizer/stats.json` | `~/.config/gmux-sessionizer/stats.json` |
| Template dir | `~/.config/tmux-sessionizer/templates` | `~/.config/gmux-sessionizer/templates` |
| Scanner binary lookup | Hardcoded paths | Search `$PATH`, then `$GMUX_SCANNER_PATH`, then relative to script |
| Binary name | `tmux-sessionizer-scan` | `gmux-scanner` |
| Personal dotfiles path | `$HOME/.homesick/repos/dotfiles/home` | Remove from defaults; document adding custom paths |
| Homeshick-specific detection | Checks for `.homesick` dir | Generalize to check for common dotfile managers or just "dotfiles" in path/remote |
| Preview: hardcoded username | `rodk` appears in examples | Remove from all examples |

### 4.2 Rust Scanner Changes (`scanner/`)

| Item | Current State | Target State |
|------|--------------|--------------|
| Package name | `tmux-sessionizer-scan` | `gmux-scanner` |
| Binary name | `tmux-sessionizer-scan` | `gmux-scanner` |
| `dirs` module | Custom `home_dir_get()` | Keep (avoids dependency) or use `dirs` crate |
| Error handling | Panics/unwraps in some places | Replace with proper error handling |
| CLI parsing | Manual arg parsing | Consider `clap` or keep manual (fewer deps) |
| Stats file default | Hardcoded path with `tmux-sessionizer` | Update to `gmux-sessionizer` |
| Tests | None | Add unit tests for frecency calc, project detection, git status |

### 4.3 Config & Templates

| Item | Current State | Target State |
|------|--------------|--------------|
| Config file | Sources as zsh | Keep — it's elegant for zsh scripts |
| Example config | Embedded in README | Ship as `config/config.example` with full comments |
| Templates | Work but are bash scripts | Document authoring, add `$SESSION` and `$PROJECT_DIR` variables |
| Template names | `*.tmux` extension | Keep — clear and descriptive |

### 4.4 Naming Consistency Pass

All references to `tmux-sessionizer` need updating:

- Script name → `gmux-sessionizer`
- Scanner binary → `gmux-scanner`
- Config directory → `~/.config/gmux-sessionizer/`
- Internal comments and error messages
- README, docs, all markdown

---

## 5. Documentation Plan

### 5.1 README.md (Complete Rewrite)

Structure:
1. **Hero section** — Name, tagline, badges (license, CI, version)
2. **Demo GIF** — Terminal recording showing fuzzy find → session switch
3. **Features** — Bullet list with screenshots/examples
4. **Quick Start** — Install in 3 commands
5. **Installation** — Detailed: Homebrew (future), manual, from source
6. **Configuration** — Config file reference, search paths, feature toggles
7. **Session Templates** — How they work, built-in templates, custom templates
8. **How It Works** — Architecture overview (zsh + Rust scanner)
9. **Frecency Algorithm** — Brief explanation with examples
10. **Keybindings** — Shell and fzf shortcuts
11. **FAQ / Troubleshooting**
12. **Credits** — "Inspired by ThePrimeagen's tmux-sessionizer", fzf, gpakosz/.tmux
13. **License**

### 5.2 Inline Documentation

- Add header comment block to zsh script with: name, version, license, description
- Add `--help` flag to the zsh script
- Add `--version` flag to both script and scanner
- Add doc comments to Rust code (rustdoc style)
- Document the zsh-Rust interface (arguments, output format, exit codes)

### 5.3 man page (Future)

Not for initial release, but plan the structure.

---

## 6. Quality & Testing

### 6.1 Rust Scanner

- [ ] Unit tests for `calculate_frecency()`
- [ ] Unit tests for `detect_project_type()`
- [ ] Unit tests for `format_last_used()`
- [ ] Unit tests for `scan_directories()` (with temp dirs)
- [ ] Unit tests for stats load/save round-trip
- [ ] Integration test: scan with mock directory tree
- [ ] `cargo clippy` clean
- [ ] `cargo fmt` applied

### 6.2 Zsh Script

- [ ] ShellCheck clean (or document known zsh-isms)
- [ ] Test: runs without config file (defaults work)
- [ ] Test: runs without Rust binary (fallback works)
- [ ] Test: handles spaces in directory names
- [ ] Test: handles missing directories gracefully

### 6.3 CI (GitHub Actions)

- Rust: `cargo build`, `cargo test`, `cargo clippy`, `cargo fmt --check`
- Shell: ShellCheck on the zsh script
- Matrix: test on Ubuntu and macOS

---

## 7. Installation Story

### 7.1 Initial Release

```bash
# Clone and install
git clone https://github.com/codelahoma/gmux-sessionizer.git
cd gmux-sessionizer
make install  # Copies script to ~/bin, builds & installs Rust binary

# Or just the script (no Rust needed)
make install-script
```

The `Makefile` should support:
- `make build` — Build Rust scanner
- `make install` — Install both script and scanner to `$PREFIX/bin` (default: `~/.local/bin`)
- `make install-script` — Install just the zsh script
- `make uninstall` — Remove installed files
- `make test` — Run all tests

### 7.2 Future Installation Methods

- **Homebrew tap**: `brew install codelahoma/tap/gmux-sessionizer`
- **Cargo**: `cargo install gmux-scanner` (scanner only)
- **AUR**: Arch Linux package (if demand exists)
- **Nix**: Nix flake

### 7.3 Post-Install Setup

The script should handle first-run gracefully:
- Auto-create `~/.config/gmux-sessionizer/` if missing
- Copy default config if none exists (or use built-in defaults)
- Print setup instructions on first run

---

## 8. Release Versioning

Use semantic versioning. Proposed initial release: **v1.0.0** (this is already
a mature, battle-tested tool).

### Changelog for v1.0.0

Features shipping in the initial open-source release:
- Frecency-based session sorting
- Project type detection (Rust, Python, Node.js, Go, Ruby, Java, dotfiles)
- Git status indicators (clean/dirty/ahead-behind)
- Rich fzf preview pane
- Session templates with per-project-type layouts
- Custom per-project templates (`.tmux-template`)
- Usage statistics tracking
- Configurable search paths and feature toggles
- Rust scanner for fast directory enumeration and git status
- Graceful fallback to pure-shell scanning

---

## 9. Migration Path (Dotfiles → Standalone Repo)

### For the Author (codelahoma)

1. Create `codelahoma/gmux-sessionizer` repo on GitHub
2. Copy files from dotfiles (not `git subtree` — clean start)
3. Apply all generalization changes
4. Tag `v1.0.0`
5. Update dotfiles to either:
   - a) Submodule the new repo and symlink, or
   - b) Install via `make install` in dotfiles bootstrap script, or
   - c) Keep a local copy that's periodically synced

### For Users

1. Clone repo
2. Run `make install`
3. Add keybinding to `.zshrc`: `bindkey -s ^F "gmux-sessionizer\n"`
4. (Optional) Add tmux binding: `bind-key -r f run-shell "tmux neww gmux-sessionizer"`
5. (Optional) Edit `~/.config/gmux-sessionizer/config` to customize paths

---

## 10. Work Phases

### Phase 1: Repository Setup
- [ ] Create GitHub repo
- [ ] Set up repo structure (as defined in §3)
- [ ] Add LICENSE (MIT)
- [ ] Add .gitignore (Rust targets, editor files)
- [ ] Add CI workflow (GitHub Actions)

### Phase 2: Code Generalization
- [ ] Rename all `tmux-sessionizer` references → `gmux-sessionizer`
- [ ] Rename scanner binary → `gmux-scanner`
- [ ] Update default paths to generic values
- [ ] Add XDG_CONFIG_HOME support
- [ ] Add `--help` and `--version` flags
- [ ] Add header comment block with license
- [ ] Clean up hardcoded personal paths

### Phase 3: Code Quality
- [ ] Add Rust unit tests
- [ ] Run `cargo clippy` and fix warnings
- [ ] Run `cargo fmt`
- [ ] Run ShellCheck on zsh script, fix what's fixable
- [ ] Review error handling in Rust code
- [ ] Ensure graceful degradation without Rust binary

### Phase 4: Documentation
- [ ] Write public README.md
- [ ] Write config reference doc
- [ ] Write template authoring guide
- [ ] Add inline code documentation
- [ ] Record terminal demo GIF (or asciicast)
- [ ] Write CONTRIBUTING.md

### Phase 5: Build & Install
- [ ] Create top-level Makefile
- [ ] Create install.sh for one-liner installs
- [ ] Test install on clean Ubuntu
- [ ] Test install on clean macOS
- [ ] Add first-run config generation

### Phase 6: Release
- [ ] Final review of all files
- [ ] Tag v1.0.0
- [ ] Write GitHub release notes
- [ ] Update dotfiles repo to reference the new project
- [ ] Announce (if desired)

---

## 11. Credits (to include in the released project)

- **Inspired by**: [ThePrimeagen's tmux-sessionizer](https://github.com/ThePrimeagen/.dotfiles)
- **Built with**: [fzf](https://github.com/junegunn/fzf) by Junegunn Choi
- **Git operations**: [libgit2](https://libgit2.org/) via the [git2-rs](https://github.com/rust-lang/git2-rs) crate
- **tmux config reference**: [gpakosz/.tmux](https://github.com/gpakosz/.tmux)
