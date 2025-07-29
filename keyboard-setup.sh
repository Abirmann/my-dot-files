#!/bin/bash

# Keyboard Layout Setup Script
# Sets up Russian/English as Group 1 and Ancient Greek as Group 2

echo "Setting up keyboard layouts..."

# Configure two layout groups:
# Group 1: Russian + English (switch with Win+Space) 
# Group 2: Ancient Greek
# Switch between groups with Ctrl+Shift+Alt+G

setxkbmap \
  -layout "ru,us,grc" \
  -variant ",," \
  -option "" \
  -option "grp:win_space_toggle" \
  -option "grp2:ctrl_shift_alt_toggle"

# Alternative: If you prefer different group switching keys:
# -option "grp2:alt_shift_toggle"     # Alt+Shift for group switching
# -option "grp2:ctrl_alt_toggle"      # Ctrl+Alt for group switching  
# -option "grp2:menu_toggle"          # Menu key for group switching

echo "Keyboard layout configured:"
echo "- Group 1: Russian ↔ English (Win+Space)"
echo "- Group 2: Ancient Greek (Ctrl+Shift+Alt)"
echo ""
echo "Current layout status:"
setxkbmap -query 