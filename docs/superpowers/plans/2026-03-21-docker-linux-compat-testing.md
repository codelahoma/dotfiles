# Docker Linux Compatibility Testing — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Docker-based Ubuntu 24.04 test harness that verifies dotfiles work on Linux, plus make `.zshenv` and `.zshrc` cross-platform.

**Architecture:** A `test/linux-compat/` directory contains a Dockerfile (Ubuntu 24.04 + prereqs), a test script (clone, link, verify), and a host wrapper. Both `.zshenv` and `.zshrc` get platform guards so they load on macOS and Linux.

**Tech Stack:** Docker, bash, zsh, emacs-nox (batch mode)

**Spec:** `docs/superpowers/specs/2026-03-21-docker-linux-compat-testing-design.md`

---

## File Structure

| File | Action | Responsibility |
|------|--------|---------------|
| `home/.zshenv` | Modify | Guard hardcoded macOS/Homebrew paths, handle missing cargo |
| `home/.zshrc` | Modify | Guard Homebrew, macOS plugins, macOS aliases, tool paths; handle Ubuntu binary names (fdfind, batcat) |
| `test/linux-compat/Dockerfile` | Create | Ubuntu 24.04 base image with all prereqs, non-root testuser |
| `test/linux-compat/test-dotfiles.sh` | Create | 4-phase verification script (clone/link, shell, emacs, tools) |
| `test/linux-compat/run.sh` | Create | Host-side wrapper: build image, pass branch, stream output |

---

### Task 1: Make `.zshenv` cross-platform

**Files:**
- Modify: `home/.zshenv`

The `.zshenv` loads before `.zshrc` and has hardcoded `/Users/rodk/` paths, Homebrew paths, and an unconditional cargo source. All of these will fail on Linux.

- [ ] **Step 1: Replace hardcoded PATH block (lines 1-7)**

Replace:
```zsh
# PATH configuration
PATH="/opt/homebrew/bin:/Users/rodk/.local/bin:~/bin:/opt/local/bin:/opt/local/sbin:~/usr/local/opt/coreutils/libexec/gnubin:/Library/Android/sdk/tools:~/Library/Android/sdk/platform-tools:$PATH"
export PATH="/Users/rodk/bin:/usr/local/opt/qt@5.5/bin:$PATH"
export PATH="/opt/homebrew/opt/sqlite/bin:$PATH"
export PATH="$PATH:/Users/rodk/.lmstudio/bin"
export PATH=/Users/rodk/Library/Python/3.9/bin:$PATH
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:/usr/local/man:$MANPATH"
```

With:
```zsh
# PATH configuration (cross-platform)
# Core paths (always present)
PATH="$HOME/.local/bin:$HOME/bin:$PATH"

# macOS-specific paths
if [[ "$OSTYPE" == darwin* ]]; then
  PATH="/opt/homebrew/bin:$PATH"
  PATH="/opt/homebrew/opt/sqlite/bin:$PATH"
  PATH="/opt/local/bin:/opt/local/sbin:$PATH"
  export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:/usr/local/man:$MANPATH"
fi

# Optional tool paths (only if they exist)
[[ -d "$HOME/.lmstudio/bin" ]] && PATH="$PATH:$HOME/.lmstudio/bin"

export PATH
```

- [ ] **Step 2: Guard HOMESHICK_DIR and macOS-specific tool vars (lines 16, 30-31)**

Replace:
```zsh
export HOMESHICK_DIR=/opt/homebrew/opt/homeshick
```

With:
```zsh
if [[ -d /opt/homebrew/opt/homeshick ]]; then
  export HOMESHICK_DIR=/opt/homebrew/opt/homeshick
elif [[ -d "$HOME/.homesick/repos/homeshick" ]]; then
  export HOMESHICK_DIR="$HOME/.homesick/repos/homeshick"
fi
```

Replace:
```zsh
export KALEIDOSCOPE_DIR=/Users/rodk/github/Kaleidoscope
export ZSH_WAKATIME_BIN=/opt/homebrew/bin/wakatime-cli
```

With:
```zsh
if [[ "$OSTYPE" == darwin* ]]; then
  export KALEIDOSCOPE_DIR="$HOME/github/Kaleidoscope"
  export ZSH_WAKATIME_BIN=/opt/homebrew/bin/wakatime-cli
fi
```

- [ ] **Step 3: Guard cargo env source (line 62)**

Replace:
```zsh
. "$HOME/.cargo/env"
```

With:
```zsh
[[ -f "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"
```

- [ ] **Step 4: Verify on macOS**

Run: `zsh -c 'source ~/.zshenv && echo ok'`
Expected: `ok` with no errors.

- [ ] **Step 5: Commit**

```bash
git add home/.zshenv
git commit -m "feat: make .zshenv cross-platform with macOS/Linux guards"
git push
```

---

### Task 2: Make `.zshrc` cross-platform

**Files:**
- Modify: `home/.zshrc`

The `.zshrc` has Homebrew paths, macOS-only plugins, macOS-only aliases, and assumes `fd`/`bat` binary names (Ubuntu uses `fdfind`/`batcat`).

- [ ] **Step 1: Guard Homebrew initialization (line 7)**

Replace:
```zsh
eval "$(/opt/homebrew/bin/brew shellenv)"
```

With:
```zsh
# Homebrew (macOS)
if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi
```

- [ ] **Step 2: Platform-split oh-my-zsh plugins (line 14)**

Replace:
```zsh
plugins=(1password brew eza fzf git github npm macos wakatime zsh-autosuggestions zoxide)
```

With:
```zsh
# Core plugins (cross-platform)
plugins=(eza fzf git github npm zsh-autosuggestions zoxide)

# macOS-only plugins
if [[ "$OSTYPE" == darwin* ]]; then
  plugins+=(1password brew macos wakatime)
fi
```

- [ ] **Step 3: Guard macOS-only aliases and handle Ubuntu binary names (lines 57-61)**

Replace:
```zsh
alias le='open -a /opt/homebrew/opt/emacs-plus/Emacs.app'
alias emc="emacsclient -nw"
alias ccat='/bin/cat'
alias cat='/opt/homebrew/bin/bat'
alias Ls="/bin/ls"
```

With:
```zsh
alias emc="emacsclient -nw"
alias ccat='/bin/cat'
alias Ls="/bin/ls"

if [[ "$OSTYPE" == darwin* ]]; then
  alias le='open -a /opt/homebrew/opt/emacs-plus/Emacs.app'
  alias cat='/opt/homebrew/bin/bat'
else
  # On Ubuntu/Debian, bat is installed as batcat
  if command -v batcat &>/dev/null; then
    alias cat='batcat'
  elif command -v bat &>/dev/null; then
    alias cat='bat'
  fi
fi
```

- [ ] **Step 4: Handle fd/fdfind binary name difference for FZF (lines 85-89)**

Replace:
```zsh
export FZF_DEFAULT_COMMAND="fd --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd --type=d --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"
```

With:
```zsh
# Determine fd and bat binary names (Ubuntu uses fdfind/batcat)
if command -v fd &>/dev/null; then
  _fd_cmd="fd"
elif command -v fdfind &>/dev/null; then
  _fd_cmd="fdfind"
fi

if command -v bat &>/dev/null; then
  _bat_cmd="bat"
elif command -v batcat &>/dev/null; then
  _bat_cmd="batcat"
fi

if [[ -n "${_fd_cmd:-}" ]]; then
  export FZF_DEFAULT_COMMAND="$_fd_cmd --hidden --strip-cwd-prefix --exclude .git"
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_ALT_C_COMMAND="$_fd_cmd --type=d --hidden --strip-cwd-prefix --exclude .git"
fi

if [[ -n "${_bat_cmd:-}" ]]; then
  export FZF_CTRL_T_OPTS="--preview '$_bat_cmd -n --color=always --line-range :500 {}'"
fi

export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"
```

- [ ] **Step 5: Update fzf completion functions (lines 100-106)**

Replace:
```zsh
_fzf_compgen_path() {
    fd --hidden --exclude .git . "$1"
}

_fzf_compgen_dir() {
    fd --type=d --hidden --exclude .git . "$1"
}
```

With:
```zsh
_fzf_compgen_path() {
    ${_fd_cmd:-fd} --hidden --exclude .git . "$1"
}

_fzf_compgen_dir() {
    ${_fd_cmd:-fd} --type=d --hidden --exclude .git . "$1"
}
```

- [ ] **Step 6: Guard `DISPLAY_MAC` and macOS-specific env vars (lines 124-125)**

Replace:
```zsh
export DISPLAY_MAC=`ifconfig en0 | grep "inet " | cut -d " " -f2`:0
export HELPDIR=/usr/local/share/zsh/help
```

With:
```zsh
if [[ "$OSTYPE" == darwin* ]]; then
  export DISPLAY_MAC=$(ifconfig en0 2>/dev/null | grep "inet " | cut -d " " -f2):0
  export HELPDIR=/usr/local/share/zsh/help
fi
```

- [ ] **Step 7: Guard Homebrew homeshick path (line 121)**

Replace:
```zsh
source /opt/homebrew/opt/homeshick/homeshick.sh
```

With:
```zsh
# Homeshick (HOMESHICK_DIR set in .zshenv)
if [[ -f /opt/homebrew/opt/homeshick/homeshick.sh ]]; then
  source /opt/homebrew/opt/homeshick/homeshick.sh
elif [[ -f "$HOME/.homesick/repos/homeshick/homeshick.sh" ]]; then
  source "$HOME/.homesick/repos/homeshick/homeshick.sh"
fi
```

- [ ] **Step 8: Guard tool activations that may not be installed (lines 108, 112, 139, 140)**

Replace:
```zsh
eval "$(fzf --zsh)"
```
With:
```zsh
if command -v fzf &>/dev/null; then
  eval "$(fzf --zsh)"
fi
```

Replace:
```zsh
eval "$(mise activate zsh)"
```
With:
```zsh
if command -v mise &>/dev/null; then
  eval "$(mise activate zsh)"
fi
```

Replace:
```zsh
source ~/fzf-git.sh/fzf-git.sh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
```
With:
```zsh
[[ -f ~/fzf-git.sh/fzf-git.sh ]] && source ~/fzf-git.sh/fzf-git.sh

# zsh-syntax-highlighting (must be last)
if [[ -f /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
  source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
elif [[ -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
  source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi
```

- [ ] **Step 9: Verify on macOS**

Run: `zsh -li -c 'echo ok'`
Expected: `ok` with no errors. Existing macOS behavior should be unchanged.

- [ ] **Step 10: Commit**

```bash
git add home/.zshrc
git commit -m "feat: make .zshrc cross-platform with macOS/Linux guards"
git push
```

---

### Task 3: Create Dockerfile

**Files:**
- Create: `test/linux-compat/Dockerfile`

- [ ] **Step 1: Write the Dockerfile**

```dockerfile
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    zsh \
    git \
    curl \
    wget \
    emacs-nox \
    tmux \
    build-essential \
    ripgrep \
    fd-find \
    fzf \
    bat \
    eza \
    jq \
    w3m \
    pass \
    gnupg \
    python3 \
    python3-pip \
    python3-venv \
    maildir-utils \
    isync \
    zoxide \
    zsh-syntax-highlighting \
    locales \
    sudo \
    && rm -rf /var/lib/apt/lists/*

# Generate locale (needed for zsh/emacs)
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Create non-root test user with zsh
RUN useradd -m -s /bin/zsh testuser

# Copy test script
COPY test-dotfiles.sh /usr/local/bin/test-dotfiles.sh
RUN chmod +x /usr/local/bin/test-dotfiles.sh

USER testuser
WORKDIR /home/testuser

ENTRYPOINT ["/usr/local/bin/test-dotfiles.sh"]
```

- [ ] **Step 2: Commit**

```bash
git add test/linux-compat/Dockerfile
git commit -m "feat: add Dockerfile for Linux compatibility testing"
git push
```

---

### Task 4: Create test script

**Files:**
- Create: `test/linux-compat/test-dotfiles.sh`

- [ ] **Step 1: Write the test script**

```bash
#!/usr/bin/env bash
# Test dotfiles on Ubuntu Linux
# Runs inside the Docker container as testuser

set -uo pipefail

BRANCH="${DOTFILES_BRANCH:-master}"
PASS=0
FAIL=0
FAILURES=()

pass() {
  echo "PASS: $1"
  PASS=$((PASS + 1))
}

fail() {
  echo "FAIL: $1"
  FAIL=$((FAIL + 1))
  FAILURES+=("$1")
}

check() {
  local description="$1"
  shift
  if "$@" >/dev/null 2>&1; then
    pass "$description"
  else
    fail "$description"
  fi
}

check_equal() {
  local description="$1"
  local expected="$2"
  local actual="$3"
  if [[ "$actual" == *"$expected"* ]]; then
    pass "$description"
  else
    fail "$description (expected: $expected, got: $actual)"
  fi
}

echo "=== Phase 1: Clone & Link ==="
echo "Branch: $BRANCH"

# Install homeshick
git clone https://github.com/andsens/homeshick.git "$HOME/.homesick/repos/homeshick"
source "$HOME/.homesick/repos/homeshick/homeshick.sh"

# Clone dotfiles directly (avoid implicit link from homeshick clone)
git clone https://github.com/codelahoma/dotfiles.git "$HOME/.homesick/repos/dotfiles"
cd "$HOME/.homesick/repos/dotfiles"
git checkout "$BRANCH"
cd "$HOME"

# Init submodules (oh-my-zsh)
git -C "$HOME/.homesick/repos/dotfiles" submodule update --init --recursive

# Install gpakosz/.tmux framework (required for .tmux.conf.local)
git clone https://github.com/gpakosz/.tmux.git "$HOME/.tmux"
ln -s "$HOME/.tmux/.tmux.conf" "$HOME/.tmux.conf"

# Link dotfiles
homeshick link --force dotfiles

pass "Clone and link dotfiles"

echo ""
echo "=== Phase 2: Shell ==="

# Test zsh loads
check "zsh loads without errors" zsh -li -c 'echo ok'

# Test oh-my-zsh present
check "oh-my-zsh is present" test -f "$HOME/.oh-my-zsh/oh-my-zsh.sh"

# Test tmux with custom config verification
tmux new-session -d -s configtest 2>/dev/null || true
sleep 1

# Most options use global scope (-g), but escape-time is set as session option
declare -A TMUX_GLOBAL_CHECKS=(
  ["prefix"]="C-a"
  ["history-limit"]="100000"
  ["mouse"]="on"
  ["mode-keys"]="vi"
  ["allow-passthrough"]="on"
)

for option in "${!TMUX_GLOBAL_CHECKS[@]}"; do
  expected="${TMUX_GLOBAL_CHECKS[$option]}"
  actual=$(tmux show -g "$option" 2>/dev/null || echo "")
  check_equal "tmux $option" "$expected" "$actual"
done

# escape-time is a session option in .tmux.conf.local
actual=$(tmux show escape-time 2>/dev/null || echo "")
check_equal "tmux escape-time" "0" "$actual"

tmux kill-session -t configtest 2>/dev/null || true

echo ""
echo "=== Phase 3: Emacs ==="

# Tangle dotspacemacs.org
check "tangle dotspacemacs.org" \
  emacs --batch -l org --eval '(org-babel-tangle-file (expand-file-name "~/dotspacemacs.org"))'

# Verify init.el was generated
check "init.el was generated" test -s "$HOME/.spacemacs.d/init.el"

# Byte-compile init.el (validates elisp syntax)
check "init.el is valid elisp" \
  emacs --batch -f batch-byte-compile "$HOME/.spacemacs.d/init.el"

# Verify private layers
check "rk-org layer present" test -d "$HOME/.emacs.d/private/rk-org"
check "rk-layout layer present" test -d "$HOME/.emacs.d/private/rk-layout"

echo ""
echo "=== Phase 4: Tools ==="

# Install mise
curl https://mise.run | sh
export PATH="$HOME/.local/bin:$PATH"
check "mise installed" command -v mise

# Verify ~/bin scripts are linked
check "~/bin is linked" test -d "$HOME/bin"
if [[ -d "$HOME/bin" ]]; then
  script_count=$(find "$HOME/bin" -type f -executable 2>/dev/null | wc -l)
  check "~/bin has executable scripts" test "$script_count" -gt 0
fi

# Verify git config
check "git config loads" git config user.name

echo ""
echo "==============================="
echo "Results: $PASS passed, $FAIL failed ($(( PASS + FAIL )) total)"
if [[ ${#FAILURES[@]} -gt 0 ]]; then
  echo "Failures:"
  for f in "${FAILURES[@]}"; do
    echo "  - $f"
  done
  exit 1
fi
echo "All tests passed!"
```

- [ ] **Step 2: Make executable**

Run: `chmod +x test/linux-compat/test-dotfiles.sh`

- [ ] **Step 3: Commit**

```bash
git add test/linux-compat/test-dotfiles.sh
git commit -m "feat: add test script for Linux dotfiles verification"
git push
```

---

### Task 5: Create host wrapper

**Files:**
- Create: `test/linux-compat/run.sh`

- [ ] **Step 1: Write the host wrapper**

```bash
#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
IMAGE_NAME="dotfiles-linux-test"
BRANCH=""

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --branch)
      BRANCH="$2"
      shift 2
      ;;
    *)
      echo "Usage: $0 [--branch <branch-name>]"
      exit 1
      ;;
  esac
done

# Default to current branch
if [[ -z "$BRANCH" ]]; then
  BRANCH=$(git -C "$REPO_DIR" rev-parse --abbrev-ref HEAD)
fi

echo "Testing dotfiles on Ubuntu 24.04 (branch: $BRANCH)"
echo ""

# Build the image
docker build -t "$IMAGE_NAME" "$SCRIPT_DIR"

# Run the tests
docker run --rm \
  -e "DOTFILES_BRANCH=$BRANCH" \
  "$IMAGE_NAME"
```

- [ ] **Step 2: Make executable**

Run: `chmod +x test/linux-compat/run.sh`

- [ ] **Step 3: Commit**

```bash
git add test/linux-compat/run.sh
git commit -m "feat: add host wrapper for Linux compatibility tests"
git push
```

---

### Task 6: End-to-end validation

**Files:** None (testing only)

- [ ] **Step 1: Push all changes**

Ensure the branch is fully pushed so the container can clone it.

- [ ] **Step 2: Run the full test suite**

Run: `./test/linux-compat/run.sh`

Expected: Docker builds the image, runs all 4 phases, reports results. Some tests may fail — this is the point of the test harness.

- [ ] **Step 3: Fix any failures**

Iterate: fix the failing config, commit, push, re-run `./test/linux-compat/run.sh`.

- [ ] **Step 4: Final commit when all tests pass**

```bash
git commit --allow-empty -m "test: all Linux compatibility tests passing"
```
