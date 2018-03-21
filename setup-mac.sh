#!/usr/bin/env bash

echo "========================================================================"
echo "|                       Ben's macOS Setup Script                       |"
echo "========================================================================"

echo "We'll need sudo permissions temporarily..."
sudo echo "Successfully used sudo!"

# Install homebrew
echo "Installing homebrew..."
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

echo "Installing wget zsh git stow tmux"

brew install wget zsh git stow tmux

echo "Installing Hack font"
wget -O font.zip https://github.com/source-foundry/Hack/releases/download/v3.003/Hack-v3.003-ttf.zip && unzip -j font.zip -d /Library/Fonts/ && rm font.zip

echo "Installing applications..."
brew tap caskroom/cask
brew cask install iterm2 firefox github-desktop keepassxc virtualbox nextcloud thunderbird flux

# Emacs. The one true editor.
echo "Installing Emacs..."
brew install emacs --devel --with-ctags --with-mailutils --with-modules --with-cocoa --with-imagemagick@6 --with-gnutls --with-librsvg && brew linkapps

# Of course, my Emacs dotfiles!
echo "Installing siraben's dotfiles"
git clone https://github.com/siraben/dotfiles ~/dotfiles
source <(cat ~/dotfiles/install.sh)

# Start in the background for configuration
echo "Installing aspell, chezscheme and llvm"
brew install llvm --with-clang

echo "Starting Emacs in the background..."
brew install aspell chezscheme
emacs --daemon

say "Done configuring your computer, for now!"

# Candidate change to automatically set up irony mode
# IRONY_PATH=`ls -1 ~/.emacs.d/elpa/ | grep ^irony | head -n 1`

# LLVM_CONFIG=`which llvm-config`

# if [ -z $LLVM_CONFIG ]; then
#   echo "llvm-config was not found. trying to find it at /usr/local..."
#   LLVM_CONFIG=`find /usr/local -type f -name llvm-config | head -n 1`
# fi

# LIBCLANG_LIBRARY=`$LLVM_CONFIG --libdir`/libclang.dylib
# LIBCLANG_INCLUDE=`$LLVM_CONFIG --includedir`

# cmake -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON \
#       -DLIBCLANG_LIBRARY=$LIBCLANG_LIBRARY \
#       -DLIBCLANG_INCLUDE_DIR=$LIBCLANG_INCLUDE \
#       -DCMAKE_INSTALL_PREFIX\=~/.emacs.d/irony/ ~/.emacs.d/elpa/$IRONY_PATH/server &&\
#     cmake --build . --use-stderr --config Release --target install

