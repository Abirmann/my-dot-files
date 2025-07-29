#!/bin/bash

echo "=== Current Keyboard State ==="
setxkbmap -query
echo

echo "=== Saved State (if any) ==="
STATE_FILE="$HOME/.local/state/keyboard-layout-state"
if [[ -f "$STATE_FILE" ]]; then
    echo "State file exists:"
    cat "$STATE_FILE"
else
    echo "No saved state found"
fi
echo

echo "=== XKB Options Explanation ==="
echo "Current options meaning:"
current_options=$(setxkbmap -query | grep options | awk '{print $2}')
if [[ -n "$current_options" ]]; then
    echo "  $current_options"
    echo "    - grp:win_space_toggle = Win+Space switches layouts"
    echo "    - Any other options preserve X11 functionality"
else
    echo "  No options set (this might break some shortcuts!)"
fi 