#!/bin/bash

# AppImage Integration Configuration
# Format: "command-name:AppImage-filename"

APPIMAGES=(
    "obsidian:Obsidian-1.8.9.AppImage"
    "cursor:Cursor-0.48.9-x86_64.AppImage"
    # Add more AppImages here:
    # "discord:Discord-1.0.0.AppImage"
    # "code:VSCode-1.0.0.AppImage"
)

# Directory where AppImages are stored
APPIMAGE_DIR="$HOME/Downloads"

# Directory where wrapper scripts will be created
WRAPPER_DIR="$HOME/.local/bin" 