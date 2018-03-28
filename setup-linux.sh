#!/usr/bin/env bash
echo "========================================================================"
echo "|                       Ben's Linux Setup Script                       |"
echo "========================================================================"

echo "We'll need sudo permissions temporarily..."
sudo echo "Successfully used sudo!" || exit

sudo add-apt-repository -y ppa:dyatlov-igor/materia-theme

sudo apt update

sudo apt install -y emacs git stow gcc zsh wget curl guile emacs25-common-non-dfsg

# Setup Oh My Zsh!

sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"


sudo apt install materia-theme
