#!/bin/bash
# Complete GTD system symlink setup

echo "Setting up complete GTD system symlinks..."

# Create GTD module directory if it doesn't exist
mkdir -p ~/.spacemacs.d/codelahoma-gtd

# Create symlinks for all GTD modules
GTD_MODULES=(
    "codelahoma-gtd-config.el"
    "codelahoma-gtd-core.el"
    "codelahoma-gtd-capture.el"
    "codelahoma-gtd-process.el"
    "codelahoma-gtd-review.el"
    "codelahoma-gtd-roam.el"
)

echo "Creating symlinks for GTD modules..."
for module in "${GTD_MODULES[@]}"; do
    ln -sf ~/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-gtd/$module ~/.spacemacs.d/codelahoma-gtd/$module
    echo "  ✓ $module"
done

# Create symlinks for UI and bridge in .spacemacs.d root
echo ""
echo "Creating symlinks for UI and bridge..."
ln -sf ~/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-bridge.el ~/.spacemacs.d/
ln -sf ~/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-ui.el ~/.spacemacs.d/
echo "  ✓ codelahoma-bridge.el"
echo "  ✓ codelahoma-ui.el"

echo ""
echo "Current .spacemacs.d GTD files:"
ls -la ~/.spacemacs.d/codelahoma-*.el
echo ""
echo "GTD module directory:"
ls -la ~/.spacemacs.d/codelahoma-gtd/

echo ""
echo "GTD system setup complete. Reload Spacemacs with 'SPC f e R'"