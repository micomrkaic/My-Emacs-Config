#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Checking required command-line tools..."

if command -v apt >/dev/null 2>&1; then
    sudo apt update
    sudo apt install -y wget unzip fontconfig
fi

echo "Creating Emacs config directories..."
mkdir -p "$HOME/.emacs.d"

echo "Backing up existing Emacs config files, if present..."

if [ -f "$HOME/.emacs" ]; then
    cp "$HOME/.emacs" "$HOME/.emacs.backup.$(date +%Y%m%d-%H%M%S)"
fi

if [ -f "$HOME/.emacs.d/init.el" ]; then
    cp "$HOME/.emacs.d/init.el" "$HOME/.emacs.d/init.el.backup.$(date +%Y%m%d-%H%M%S)"
fi

echo "Installing .emacs..."
cp "$REPO_DIR/.emacs" "$HOME/.emacs"

if [ -f "$REPO_DIR/morpheus-small.png" ]; then
    echo "Installing dashboard image..."
    cp "$REPO_DIR/morpheus-small.png" "$HOME/.emacs.d/morpheus-small.png"
fi

echo "Ensuring custom.el exists..."
touch "$HOME/.emacs.d/custom.el"

echo "Done."
echo

echo "Checking for Victor Mono..."

if fc-match "Victor Mono" | grep -qi "VictorMono\|Victor Mono"; then
    echo "Victor Mono is already installed."
else
    echo "Installing Victor Mono..."

    mkdir -p "$HOME/.local/share/fonts/victor-mono"

    TMPDIR="$(mktemp -d)"
    trap 'rm -rf "$TMPDIR"' EXIT

    wget -O "$TMPDIR/VictorMonoAll.zip" \
        https://github.com/rubjo/victor-mono/raw/master/public/VictorMonoAll.zip

    unzip -o "$TMPDIR/VictorMonoAll.zip" -d "$HOME/.local/share/fonts/victor-mono"

    fc-cache -fv "$HOME/.local/share/fonts"

    echo "Victor Mono installed."
fi

echo "Start Emacs now. On first launch it will install packages automatically."
