#!/bin/bash

# Toggle between ru/us layouts and Greek layout
# Preserves X11 keyboard state to avoid breaking shortcuts

# State files to remember keyboard settings
STATE_DIR="$HOME/.local/state"
mkdir -p "$STATE_DIR"
LAYOUT_STATE="$STATE_DIR/keyboard-layout-state"

# Check current layout
current_layout=$(setxkbmap -query | grep layout | awk '{print $2}')

if [[ "$current_layout" == *"gr"* ]]; then
    # Currently in Greek, restore previous state
    if [[ -f "$LAYOUT_STATE" ]]; then
        # Restore saved state
        source "$LAYOUT_STATE"
        setxkbmap -layout "$SAVED_LAYOUT" -variant "$SAVED_VARIANT" -option "$SAVED_OPTIONS"
    else
        # Fallback to default ru/us
        setxkbmap -layout 'ru,us' -variant ',' -option '' -option 'grp:win_space_toggle'
    fi
    rm -f "$LAYOUT_STATE"
    notify-send "Keyboard" "Russian/English mode" -t 1000 2>/dev/null || echo "Switched to Russian/English"
else
    # Currently in ru/us, save state and switch to Greek
    
    # Save current keyboard state
    current_query=$(setxkbmap -query)
    current_variant=$(echo "$current_query" | grep variant | awk '{print $2}')
    current_options=$(echo "$current_query" | grep options | awk '{print $2}')
    
    cat > "$LAYOUT_STATE" << EOF
SAVED_LAYOUT="$current_layout"
SAVED_VARIANT="$current_variant"
SAVED_OPTIONS="$current_options"
EOF
    
    # Switch to Greek while preserving most options (only remove layout group options)
    # Remove only layout switching options, keep everything else
    preserved_options=$(echo "$current_options" | sed 's/grp:[^,]*,\?//g' | sed 's/,$//')
    
    if [[ -n "$preserved_options" ]]; then
        setxkbmap -layout 'gr' -variant 'polytonic' -option '' -option "$preserved_options"
    else
        setxkbmap -layout 'gr' -variant 'polytonic' -option ''
    fi
    
    notify-send "Keyboard" "Ancient Greek (Polytonic)" -t 1000 2>/dev/null || echo "Switched to Ancient Greek (Polytonic)"
fi 