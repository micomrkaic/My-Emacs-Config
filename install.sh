#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

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
echo "Start Emacs now. On first launch it will install packages automatically."
