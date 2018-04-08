#!/usr/bin/env bash
>echo "========================================================================"
echo "|                       Ben's Linux Setup Script                       |"
echo "========================================================================"

echo "We'll need sudo permissions temporarily..."
sudo echo "Successfully used sudo!" || exit

# Verify Guix download
wget ftp://alpha.gnu.org/gnu/guix/guix-binary-0.14.0.x86_64-linux.tar.xz
wget ftp://alpha.gnu.org/gnu/guix/guix-binary-0.14.0.x86_64-linux.tar.xz.sig
gpg --verify guix-binary-0.14.0.x86_64-linux.tar.xz.sig

# Extract Guix
sudo cd /tmp
sudo tar --warning=no-timestamp -xf guix-binary-0.14.0.x86_64-linux.tar.xz
sudo mv var/guix /var/ && mv gnu /
sudo ln -sf /var/guix/profiles/per-user/root/guix-profile ~root/.guix-profile


sudo GUIX_PROFILE=$HOME/.guix-profile
sudo source $GUIX_PROFILE/etc/profile

# Make Guix avaliable for everyone.
sudo mkdir -p /usr/local/bin
sudo cd /usr/local/bin
sudo ln -s /var/guix/profiles/per-user/root/guix-profile/bin/guix

# Make info manual avaliable to everyone.
sudo mkdir -p /usr/local/share/info
sudo cd /usr/local/share/info
sudo for i in /var/guix/profiles/per-user/root/guix-profile/share/info/* ; do ln -s $i ; done

# Fix locale issue.
guix package -i glibc-locales
export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale

# Install Guix packages
guix package -i guile emacs icecat stow git zsh make gcc font-hack

# Setup Oh My Zsh!
sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

echo "========================================================================"
echo "|                                 Done                                 |"
echo "========================================================================"
