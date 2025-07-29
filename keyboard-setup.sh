#!/bin/bash

# Keyboard Layout Setup Script
# Sets up Russian/English as default with Greek toggle capability

echo "Setting up default keyboard layouts..."

# Set up Russian + English (switch with Win+Space)
setxkbmap -layout 'ru,us' -variant ',' -option '' -option 'grp:win_space_toggle'

echo "Default keyboard layout configured:"
echo "- Russian ↔ English (Win+Space)"
echo "- Use ./toggle-greek.sh to switch to/from Greek"
echo ""
echo "Current layout status:"
setxkbmap -query 