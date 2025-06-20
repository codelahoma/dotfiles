#!/bin/bash
# Setup GTD system symlinks

echo "Setting up GTD system symlinks..."

# Create symlinks for bridge and ui in .spacemacs.d
cd ~/.spacemacs.d
ln -sf ~/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-bridge.el .
ln -sf ~/.homesick/repos/dotfiles/home/.spacemacs.d/codelahoma-ui.el .

echo "Symlinks created. Current .spacemacs.d GTD files:"
ls -la ~/.spacemacs.d/codelahoma-*.el
ls -la ~/.spacemacs.d/codelahoma-gtd/

echo ""
echo "GTD system setup complete. Reload Spacemacs with 'SPC f e R'"