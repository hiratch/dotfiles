#!/bin/sh
set -e

# ==========================================
# Check if running with root privileges (sudo)
# ==========================================
# 'id -u' returns the effective user ID (0 for root)
if [ "$(id -u)" -ne 0 ]; then
    echo "Error: Root privileges are required to apply system-wide changes."
    echo "Usage: sudo $0"
    exit 1
fi

echo "=== Starting system requirements installation ==="

OS="$(uname -s)"
if [ "$OS" = "Linux" ]; then
    # Check if apt-get is available
    if command -v apt-get >/dev/null 2>&1; then
        echo "Ubuntu/Debian-based Linux detected. Running apt-get..."
        
        # sudo is not required here because the script is already running as root
        apt-get update
        apt-get install -y build-essential curl zsh tmux byobu emacs vim
        
        echo "System requirements installation successfully completed."
    else
        echo "Error: This script currently only supports apt (Debian/Ubuntu-based systems)."
        exit 1
    fi
else
    echo "Error: Unsupported OS ($OS)."
    exit 1
fi
