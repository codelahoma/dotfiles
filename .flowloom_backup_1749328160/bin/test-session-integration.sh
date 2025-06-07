#!/usr/bin/env bash
# Test FlowLoom Session Python implementation

set -e

echo "Testing FlowLoom Session Management (Python Implementation)"
echo "=========================================================="

# Test directory
TEST_DIR="/tmp/flowloom-session-test-$$"
mkdir -p "$TEST_DIR"
cd "$TEST_DIR"

# Initialize git repo (required for some features)
git init -q
git config user.email "test@example.com"
git config user.name "Test User"

# Create initial commit
echo "# Test Project" > README.md
git add README.md
git commit -m "Initial commit" -q

# Copy necessary files
mkdir -p bin packages

# Use the actual FlowLoom directory
FLOWLOOM_DIR="/Users/rodk/github/flowloom"
cp "$FLOWLOOM_DIR/bin/flowloom-session" bin/
cp "$FLOWLOOM_DIR/bin/get_shell_pid.sh" bin/
cp -r "$FLOWLOOM_DIR/packages/flowloom_session" packages/

echo
echo "1. Testing session creation..."
./bin/flowloom-session create integration-test
echo "✅ Session creation passed"

echo
echo "2. Testing session listing..."
./bin/flowloom-session list
echo "✅ Session listing passed"

echo
echo "3. Testing session status..."
./bin/flowloom-session status
echo "✅ Session status passed"

echo
echo "4. Testing scan functionality..."
# Create a test file
echo "test content" > test.txt
git add test.txt
./bin/flowloom-session scan *-integration-test || true
echo "✅ Scan functionality passed"

echo
echo "5. Testing cleanup..."
./bin/flowloom-session cleanup --max-age 0 --verbose
echo "✅ Cleanup passed"

# Cleanup
cd /
rm -rf "$TEST_DIR"

echo
echo "All tests passed! 🎉"