# Docker Linux Compatibility Testing

## Purpose

Provide a Docker-based Ubuntu environment that Claude (or any CI tool) can use to verify dotfiles work correctly on Linux. This ensures all cross-platform configuration changes are validated before being deployed to VMs, containers, or other Linux hosts managed via homeshick.

## Scope

**In scope:** Shell (zsh, oh-my-zsh, tmux), Emacs (Spacemacs, tangling, private layers), CLI tools (mise, ~/bin scripts, git config), mu4e/mbsync config structure.

**Out of scope:** macOS-only tools (Hammerspoon, osx layer), credential-dependent features (mail sync, GPG decryption, pass secrets), GUI Emacs, language runtimes beyond what mise bootstraps.

**Prerequisite:** The `.zshrc` must be made cross-platform before the shell tests can pass. It currently has hardcoded Homebrew paths, macOS-specific oh-my-zsh plugins, and unconditional tool activations (fzf, mise, zoxide) that will fail on a fresh Linux container. Making `.zshrc` portable is part of the broader `feat/linux-compatibility` branch work and should be completed before or alongside this Docker test harness.

## File Layout

```
test/linux-compat/
├── Dockerfile          # Ubuntu 24.04 with all prerequisites
├── test-dotfiles.sh    # Runs inside container: clone, link, verify
└── run.sh              # Host-side wrapper: build image, run tests
```

## Dockerfile

**Base image:** `ubuntu:24.04`

**Packages:**

| Category | Packages |
|----------|----------|
| Shell | zsh, git, curl, wget |
| Emacs | emacs-nox |
| Terminal | tmux |
| Build | build-essential |
| Utilities | ripgrep, fd-find, fzf, bat, eza, jq, w3m, pass, gnupg |
| Python | python3, python3-pip, python3-venv |
| Email (config only) | maildir-utils (provides `/usr/bin/mu`), isync |
| Navigation | zoxide |

**User setup:** Non-root `testuser` with zsh as default shell and a home directory at `/home/testuser`.

**Not included:** Node.js, Rust, Go, or other language runtimes. These are installed via mise after dotfiles are linked, which is itself part of the test.

## Test Script (`test-dotfiles.sh`)

Runs inside the container as `testuser`. Each check prints `PASS` or `FAIL` with a description. The script exits non-zero if any check fails. A summary line at the end reports `X/Y tests passed` with a list of failures.

### Phase 1 — Clone & Link

1. Install homeshick: `git clone https://github.com/andsens/homeshick.git ~/.homesick/repos/homeshick`
2. Clone dotfiles: `homeshick clone codelahoma/dotfiles` then `cd ~/.homesick/repos/dotfiles && git checkout $BRANCH`
3. Initialize submodules: `git -C ~/.homesick/repos/dotfiles submodule update --init --recursive`
4. Link dotfiles: `homeshick link --force dotfiles`

### Phase 2 — Shell

5. Verify zsh loads without errors: `zsh -li -c 'echo ok'`
6. Verify oh-my-zsh is present: `~/.oh-my-zsh/oh-my-zsh.sh` exists
7. Verify tmux config loads: `tmux new-session -d -s test && tmux kill-session -t test`

### Phase 3 — Emacs

8. Tangle `~/dotspacemacs.org` and verify `~/.spacemacs.d/init.el` is generated: `emacs --batch -l org --eval '(org-babel-tangle-file (expand-file-name "~/dotspacemacs.org"))'`
9. Verify tangled init.el is valid elisp (byte-compiles without error): `emacs --batch -f batch-byte-compile ~/.spacemacs.d/init.el`. Note: this validates syntax, not runtime loading — Spacemacs itself is not installed in the container.
10. Verify testable private layers are present under `~/.emacs.d/private/`: `rk-org/` and `rk-layout/` (real directories in the repo). The `gtd-zettelkasten/` layer is a symlink to an external repo (`~/github/org-gtd-zettelkasten/...`) and will be a broken symlink — this is expected and should be skipped.

### Phase 4 — Tools

11. Install mise and verify it initializes
12. Verify `~/bin` scripts are linked and executable
13. Verify git config loads: `git config user.name` returns a value

### Not Tested

- Anything requiring credentials (mail sync, GPG key decryption, pass)
- Hammerspoon or other macOS-only tools
- GUI Emacs features
- Full Spacemacs package installation (too slow for a verification test)
- `gtd-zettelkasten` layer (external symlink, expected to be broken)

## Host Wrapper (`run.sh`)

A bash script invoked from the repo root or directly:

```bash
# Test current branch (auto-detected)
./test/linux-compat/run.sh

# Test a specific branch
./test/linux-compat/run.sh --branch master
```

Behavior:

1. Detect the current git branch (or accept `--branch <name>` override)
2. Build the Docker image (tagged `dotfiles-linux-test:latest`), leveraging layer cache
3. Run the container, passing the branch name as an environment variable
4. Stream test output to stdout
5. Exit with the container's exit code

## Design Decisions

**Clone from GitHub (not bind mount):** The container clones the repo from GitHub, mirroring the real-world flow of setting up a new Linux host. This means the branch must be pushed before testing. This is intentional — it validates the same path a user would follow.

**emacs-nox (not full Emacs):** The container only needs to tangle org files and batch-load config. No GUI is needed, and emacs-nox is significantly smaller.

**No language runtimes pre-installed:** Mise manages language runtimes. Pre-installing them would bypass the actual setup path and hide potential issues.

**Non-root user:** Dotfiles are meant to be applied as a regular user. Running as root would mask permission issues and path assumptions.

**Byte-compile instead of batch-load for Emacs test:** The tangled `init.el` defines Spacemacs hook functions (`dotspacemacs/layers`, `dotspacemacs/user-init`, etc.) that require Spacemacs to call them. Byte-compiling validates elisp syntax without needing the full Spacemacs runtime.

**Package name: `maildir-utils` not `mu`:** On Ubuntu 24.04, the mu binary is provided by the `maildir-utils` package, not a package called `mu`.
