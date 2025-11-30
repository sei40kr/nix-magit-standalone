# nix-magit-standalone

A Nix Flake that provides standalone Magit with Evil (Vim keybindings) support.

## Features

- Easy to use Magit without Emacs knowledge
- Evil (Vim keybindings) enabled by default
- Simple installation via Nix Flake
- Lightning-fast startup using Emacs portable dumper (pdumper) - heavily optimized for instant launch
- External editor support via vterm - opens your `$GIT_EDITOR`/`$EDITOR` in a transparent fullscreen terminal buffer for commit messages, rebase scripts, and other Git operations (uses a hacky but effective approach)

## Usage

### Installation

```bash
# Install (Evil mode enabled by default)
nix profile install github:sei40kr/nix-magit-standalone
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

## Performance

Thanks to pdumper optimization, nix-magit-standalone achieves blazing-fast startup times:

**Startup time: ~20ms** (average of 5 runs)

```bash
# Measurement command
for i in 1 2 3 4 5; do
  echo "Run $i:"
  (time timeout 5 ./result/bin/magit --eval '(kill-emacs)' 2>&1 >/dev/null) 2>&1 | tail -1
done
```

Results:
```
Run 1: 0.025 total
Run 2: 0.018 total
Run 3: 0.017 total
Run 4: 0.019 total
Run 5: 0.020 total
```

Measured on:
- CPU: Intel Core i7-12700K @ 5.00 GHz
- OS: NixOS 25.05 (Warbler)
- Kernel: Linux 6.12.30

This is significantly faster than standard Emacs startup, making it feel instant and perfect for quick Git operations.

## Credits

This project uses the following excellent Emacs packages:

- [Magit](https://magit.vc/) - A Git porcelain inside Emacs
- [vertico.el](https://github.com/minad/vertico) - Vertical completion UI
- [clipetty](https://github.com/spudlyo/clipetty) - OSC 52 clipboard integration for terminal Emacs
- [Evil](https://github.com/emacs-evil/evil) - Extensible vi layer for Emacs (optional)
- [Evil Collection](https://github.com/emacs-evil/evil-collection) - Collection of Evil bindings for various modes (optional)

## License

GPL-3.0-or-later
