#!/usr/bin/env bash

## License:

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

control_c()
{
  (>&2 printf "\n${RED}Aborting...${NC}\n")
  exit 1
}

trap control_c SIGINT

echo "========================================================================"
echo "|                     Ben's macOS Defaults Script                      |"
echo "========================================================================"

echo "Some defaults write operations need sudo, asking for permission..."
sudo echo "Sucessfully used sudo!" || exit

# Setup defaults write in macOS
echo "Disabling press and hold..."
defaults write -g ApplePressAndHoldEnabled -bool false
echo "Disabling Dock launch animation..."
defaults write com.apple.dock launchanim -bool false

# Set a blazingly fast keyboard repeat rate
echo "Setting a high keyboard repeat rate..."
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain InitialKeyRepeat -int 25

echo "Setting locale..."
defaults write NSGlobalDomain AppleLanguages -array "en"
defaults write NSGlobalDomain AppleLocale -string "en_US@currency=THB"
defaults write NSGlobalDomain AppleMeasurementUnits -string "Centimeters"
defaults write NSGlobalDomain AppleMetricUnits -bool true

# Finder: allow quitting via ⌘ + Q; doing so will also hide desktop icons
echo "Enabling quit Finder..."
defaults write com.apple.finder QuitMenuItem -bool true

# Finder: disable window animations and Get Info animations
echo "Disabling window animations and Get Info animations..."
defaults write com.apple.finder DisableAllAnimations -bool true


echo "Disabling shadow in screenshots"
defaults write com.apple.screencapture disable-shadow -bool true


echo "Increasing window resize speed for Cocoa applications..."
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001

echo "Expanding save panel by default..."
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

echo "Expanding print panel by default..."
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

echo "Saving to disk (not to iCloud) by default..."
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

echo "Automatically quiting printer app once the print jobs complete..."
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true


# Hmm... setting this thing seems to make the script hang.
# echo "Removing duplicates in the “Open With” menu (also see lscleanup alias)..."
# /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user

# Set Help Viewer windows to non-floating mode
defaults write com.apple.helpviewer DevMode -bool true


echo "Reveal IP address, hostname, OS version, etc. when clicking the clock in login window..."
sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

echo "Restarting automatically if the computer freezes..."
sudo systemsetup -setrestartfreeze on

echo "Showing all filename extensions..."
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

echo "Disabling the warning when changing a file extension"
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

echo "Avoiding creating .DS_Store files on network"
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

echo "Showing the ~/Library folder..."
chflags nohidden ~/Library

echo "Showing the /Volumes folder..."
sudo chflags nohidden /Volumes

echo "Changing minimize/maximize window effect..."
defaults write com.apple.dock mineffect -string "scale"

echo "Speeding up Mission Control animations..."
defaults write com.apple.dock expose-animation-duration -float 0.1


echo "Stop showing windows by application in Mission Control..."
defaults write com.apple.dock expose-group-by-app -bool false

echo "Disabling Dashboard..."
defaults write com.apple.dashboard mcx-disabled -bool true

echo "Removing dashboard as a space..."
defaults write com.apple.dock dashboard-in-overlay -bool true

echo "Stopping automatic rearrangement of Spaces based on most recent use..."
defaults write com.apple.dock mru-spaces -bool false

echo "Removing the auto-hiding Dock delay..."
defaults write com.apple.dock autohide-delay -float 0

echo "Removing the animation when hiding/showing the Dock..."
defaults write com.apple.dock autohide-time-modifier -float 0

echo  "Automatically hiding and showing the Dock..."
defaults write com.apple.dock autohide -bool true

echo "Using plain text mode for new TextEdit documents"
defaults write com.apple.TextEdit RichText -int 0
echo "Opening and saving files as UTF-8 in TextEdit..."
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4

echo "Disabling Notification Center and remove the menu bar icon..."
launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null

echo "Disabling automatic capitalization as it's annoying when typing code..."
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false

echo "Disabling smart dashes as they're annoying when typing code..."
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

echo "Disabling automatic period substitution as it's annoying when typing code..."
defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false

echo "Disabling smart quotes as they’re annoying when typing code..."
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

echo "Disabling auto-correct..."
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false


echo "Trackpad: enabling tap to click for this user and for the login screen"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Increase sound quality for Bluetooth headphones/headsets
defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40



echo "========================================================================"
echo "|                               DONE!                                  |"
echo "========================================================================"
