#!/bin/bash

# FlowLoom Environment Validation
# Validates the current FlowLoom environment configuration

set -e

echo "🔍 FlowLoom Environment Validation"
echo "================================="
echo

# Check environment variable
if [ -n "$FLOWLOOM_WORK_DIR" ]; then
    echo "🔧 FLOWLOOM_WORK_DIR is set to: '$FLOWLOOM_WORK_DIR'"
    
    # Determine configuration type
    case "$FLOWLOOM_WORK_DIR" in
        ".")
            echo "   📋 Configuration: FlowLoom Development Mode"
            ;;
        ".meta-claude")
            echo "   📋 Configuration: Explicit Default Installation"
            ;;
        *)
            echo "   📋 Configuration: Custom Installation"
            ;;
    esac
else
    echo "🔧 FLOWLOOM_WORK_DIR is unset (using default)"
    echo "   📋 Configuration: Standard Installation"
fi

echo

# Resolve actual work directory
WORK_DIR="${FLOWLOOM_WORK_DIR:-.meta-claude}"
echo "📂 Resolved work directory: $WORK_DIR"

# Check if work directory exists
if [ -d "$WORK_DIR" ]; then
    echo "   ✅ Work directory exists"
else
    echo "   ⚠️  Work directory does not exist: $WORK_DIR"
    echo "   💡 This is normal for fresh installations or custom directories"
fi

echo

# Check for FlowLoom structure
echo "🏗️  FlowLoom Structure Check:"

# Core directories
core_dirs=("plans" "docs" "bin")
missing_dirs=()

for dir in "${core_dirs[@]}"; do
    if [ -d "$WORK_DIR/$dir" ]; then
        echo "   ✅ $WORK_DIR/$dir"
    else
        echo "   ❌ $WORK_DIR/$dir (missing)"
        missing_dirs+=("$dir")
    fi
done

# Optional directories
optional_dirs=("backup" "prompts" "walkthroughs")
echo
echo "📚 Optional Structure:"

for dir in "${optional_dirs[@]}"; do
    if [ -d "$WORK_DIR/$dir" ]; then
        echo "   ✅ $WORK_DIR/$dir"
    else
        echo "   ⚪ $WORK_DIR/$dir (optional)"
    fi
done

echo

# Overall assessment
if [ ${#missing_dirs[@]} -eq 0 ]; then
    echo "🎉 FlowLoom environment is properly configured!"
    
    # Count some content
    if [ -d "$WORK_DIR/plans" ]; then
        plan_count=$(find "$WORK_DIR/plans" -name "*.md" -type f 2>/dev/null | wc -l | tr -d ' ')
        echo "   📊 Found $plan_count plan files"
    fi
    
    if [ -d "$WORK_DIR/docs" ]; then
        doc_count=$(find "$WORK_DIR/docs" -name "*.md" -type f 2>/dev/null | wc -l | tr -d ' ')
        echo "   📊 Found $doc_count documentation files"
    fi
    
else
    echo "⚠️  Missing core directories: ${missing_dirs[*]}"
    echo "   💡 Run FlowLoom setup or installation to create missing structure"
fi

echo

# Test basic command functionality
echo "🧪 Testing Command Functionality:"

# Test plan listing (if plans directory exists)
if [ -d "$WORK_DIR/plans" ]; then
    echo "   Testing plan discovery..."
    if find "$WORK_DIR/plans" -name "*.md" -type f >/dev/null 2>&1; then
        echo "   ✅ Plan file discovery works"
    else
        echo "   ⚠️  Plan file discovery failed"
    fi
else
    echo "   ⚪ Skipping plan tests (no plans directory)"
fi

echo
echo "✨ Validation complete!"

# Exit with appropriate code
if [ ${#missing_dirs[@]} -eq 0 ]; then
    exit 0
else
    exit 1
fi