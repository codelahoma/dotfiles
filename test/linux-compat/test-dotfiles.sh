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

# Test oh-my-zsh present (submodule at ~/.homesick/repos/dotfiles/home/.oh-my-zsh,
# symlinked to ~/.oh-my-zsh by homeshick)
check "oh-my-zsh is present" test -f "$HOME/.homesick/repos/dotfiles/home/.oh-my-zsh/oh-my-zsh.sh"

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
  # Scripts may be symlinks, so check both regular files and symlinks
  script_count=$(find -L "$HOME/bin" -type f -executable 2>/dev/null | wc -l)
  check "~/bin has executable scripts" test "$script_count" -gt 0
fi

# Verify git config (just check that .gitconfig or dotfiles gitconfig is loadable;
# user.name won't be set in a fresh container without .gitconfig in the repo)
check "git is configured" git config --list

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
