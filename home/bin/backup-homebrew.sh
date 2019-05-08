#!/bin/bash

brew_command=/usr/local/bin/brew
brew_cask_command="$brew_command cask"

echo '#!/bin/bash'
echo ''
echo 'trap ctrl_c INT'
echo 'function ctrl_c() {'
echo 'echo "** Trapped CTRL-C"'
echo 'exit 1'
echo '}'
echo ''
echo 'brew_command=/usr/local/bin/brew'
echo 'brew_cask_command="$brew_command cask"'
echo 'failed_items=""'
echo 'function install_package() {'
echo 'echo EXECUTING: brew install $1 $2'
echo '$brew_command install $1 $2'
echo '[ $? -ne 0 ] && $failed_items="$failed_items $1"  # package failed to install.'
echo '}'
echo 'function install_cask_package() {'
echo 'echo EXECUTING: brew cask install $1'
echo '$brew_cask_command install $1'
echo '[ $? -ne 0 ] && $failed_items="$failed_items $1"  # package failed to install.'
echo '}'

$brew_command tap | while read tap; do echo "$brew_command tap $tap"; done

$brew_command list | while read item;
do
  echo "install_package $item '$($brew_command info $item | /usr/bin/grep 'Built from source with:' | /usr/bin/sed 's/^[ \t]*Built from source with:/ /g; s/\,/ /g')'"
done

$brew_cask_command list | while read item;
do
  echo "install_cask_package $item"
done

echo '[ ! -z $failed_items ] && echo The following items were failed to install: && echo $failed_items'
