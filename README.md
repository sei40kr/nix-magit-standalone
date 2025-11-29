# nix-magit

A Nix Flake that provides standalone Magit with Evil (Vim keybindings) support.

## Features

- Easy to use Magit without Emacs knowledge
- Evil (Vim keybindings) enabled by default
- Simple installation via Nix Flake

## Usage

### Installation

```bash
# Install (Evil mode enabled by default)
nix profile install github:sei40kr/nix-magit
```

### Running

```bash
# Run in any directory
magit
```

## Package Options

- `evil` (boolean): Enable Evil mode (Vim keybindings), default: true
- `theme` (string): Doom Emacs theme to use, default: "doom-one"

Available themes include: `doom-one`, `doom-dracula`, `doom-gruvbox`, `doom-monokai-classic`, `doom-nord`, `doom-solarized-dark`, `doom-solarized-light`, `doom-tokyo-night`, and many more. See [doom-themes](https://github.com/doomemacs/themes) for the full list.

### Customization with override

You can customize the package using override:

```nix
# Disable Evil mode
magit-standalone.override {
  evil = false;
};

# Change theme
magit-standalone.override {
  theme = "doom-gruvbox";
};
```

## Known Issues

1. Cannot open editor through server socket, so all git operations that require an editor (including commits) are unavailable

## Credits

This project uses the following excellent Emacs packages:

- [Magit](https://magit.vc/) - A Git porcelain inside Emacs
- [vertico.el](https://github.com/minad/vertico) - Vertical completion UI
- [Evil](https://github.com/emacs-evil/evil) - Extensible vi layer for Emacs (optional)
- [Evil Collection](https://github.com/emacs-evil/evil-collection) - Collection of Evil bindings for various modes (optional)

## License

GPL-3.0-or-later
