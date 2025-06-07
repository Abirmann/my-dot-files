#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/config.sh"

echo "Setting up AppImage integration..."

mkdir -p "$APPIMAGE_DIR" "$WRAPPER_DIR"

if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    echo "Warning: ~/.local/bin not in PATH"
    echo "Add to shell config: export PATH=\"\$HOME/.local/bin:\$PATH\""
    echo
fi

for entry in "${APPIMAGES[@]}"; do
    IFS=':' read -r cmd_name appimage_file <<< "$entry"
    
    appimage_path="$APPIMAGE_DIR/$appimage_file"
    wrapper_path="$WRAPPER_DIR/$cmd_name"
    
    echo "Processing: $cmd_name"
    
    if [[ ! -f "$appimage_path" ]]; then
        echo "  Error: $appimage_path not found"
        continue
    fi
    
    [[ ! -x "$appimage_path" ]] && chmod +x "$appimage_path"
    
    cat > "$wrapper_path" << EOF
#!/bin/bash
exec "$appimage_path" "\$@"
EOF
    
    chmod +x "$wrapper_path"
    echo "  Created: $cmd_name"
done

[[ -f "$HOME/.cache/dmenu_run" ]] && rm -f "$HOME/.cache/dmenu_run"

echo
echo "Setup complete!"
echo "Edit $SCRIPT_DIR/config.sh to add more AppImages" 