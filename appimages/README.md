# AppImage Integration for dmenu

## Problem
AppImages don't appear in dmenu because they have complex names and aren't in PATH.

## Solution
Creates simple wrapper scripts in `~/.local/bin` that dmenu can find.

## Usage
1. Put AppImages in `~/.local/bin/`
2. Edit `config.sh` with your AppImage names
3. Run `./setup.sh`
4. Use dmenu normally

## How it works
```
dmenu → scans PATH → finds ~/.local/bin/obsidian → runs wrapper → launches AppImage
``` 