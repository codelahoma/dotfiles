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
