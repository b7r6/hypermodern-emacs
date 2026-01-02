#!/usr/bin/env bash
# hypermodern emacs installer (non-Nix)
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EMACS_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/emacs"
EMACS_D="$HOME/.emacs.d"

echo "// hypermodern emacs installer //"
echo ""

# Check for emacs
if ! command -v emacs &>/dev/null; then
    echo "error: emacs not found in PATH"
    echo "install emacs 29+ first (brew install emacs / apt install emacs / etc)"
    exit 1
fi

EMACS_VERSION=$(emacs --version | head -1 | grep -oE '[0-9]+\.[0-9]+' | head -1)
MAJOR_VERSION=$(echo "$EMACS_VERSION" | cut -d. -f1)

if [ "$MAJOR_VERSION" -lt 29 ]; then
    echo "warning: emacs $EMACS_VERSION detected, 29+ recommended for tree-sitter"
fi

# Backup existing config
backup_if_exists() {
    local path="$1"
    if [ -e "$path" ] && [ ! -L "$path" ]; then
        local backup="${path}.backup.$(date +%Y%m%d%H%M%S)"
        echo "backing up $path -> $backup"
        mv "$path" "$backup"
    elif [ -L "$path" ]; then
        echo "removing existing symlink: $path"
        rm "$path"
    fi
}

echo ""
echo "setting up config..."

# Create ~/.config/emacs structure (XDG preferred)
mkdir -p "$EMACS_DIR"

# Symlink core files
backup_if_exists "$EMACS_DIR/init.el"
backup_if_exists "$EMACS_DIR/early-init.el"
backup_if_exists "$EMACS_DIR/lib"
backup_if_exists "$EMACS_DIR/themes"

ln -sf "$REPO_DIR/init.el" "$EMACS_DIR/init.el"
ln -sf "$REPO_DIR/early-init.el" "$EMACS_DIR/early-init.el"
ln -sf "$REPO_DIR/lib" "$EMACS_DIR/lib"
ln -sf "$REPO_DIR/themes" "$EMACS_DIR/themes"

# Also link to ~/.emacs.d for compatibility
mkdir -p "$EMACS_D"
backup_if_exists "$EMACS_D/early-init.el"

ln -sf "$REPO_DIR/early-init.el" "$EMACS_D/early-init.el"

# Add lib to load-path via site-start
SITE_START="$EMACS_DIR/site-start.el"
cat > "$SITE_START" << EOF
;;; site-start.el --- hypermodern load-path setup -*- lexical-binding: t; -*-
(add-to-list 'load-path "$REPO_DIR/lib")
(add-to-list 'custom-theme-load-path "$REPO_DIR/themes")
EOF

echo ""
echo "done! start emacs and packages will be installed automatically."
echo ""
echo "tip: set OPENROUTER_API_KEY for gptel AI features:"
echo "  export OPENROUTER_API_KEY=sk-or-..."
echo ""
echo "or add to ~/.authinfo.gpg:"
echo "  machine openrouter.ai login apikey password sk-or-..."
