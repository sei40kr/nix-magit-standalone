{
  lib,
  stdenv,
  emacsPackages,
  emacs,
  makeWrapper,
  writeText,
  git,
  evil ? true,
  theme ? "doom-one",
}:

let
  emacsNoGui = emacs.override {
    withNativeCompilation = false;
    noGui = true;
    withMailutils = false;
    withTreeSitter = false;
    withWebP = false;
  };
  emacsWithPackages = emacsNoGui.pkgs.withPackages (
    epkgs:
    [
      epkgs.clipetty
      epkgs.doom-themes
      epkgs.magit
      epkgs.vertico
    ]
    ++ lib.optionals evil [
      epkgs.evil
      epkgs.evil-collection
    ]
  );

  configToDump = writeText "dump.el" ''
    ;; Use separate directories to avoid conflicts with regular Emacs
    (setq user-emacs-directory (expand-file-name "~/.local/share/magit-standalone/"))

    (defun display-startup-echo-area-message ())

    ;; Disable UI elements
    (push '(menu-bar-lines . 0) default-frame-alist)
    (push '(tool-bar-lines . 0) default-frame-alist)
    (push '(vertical-scroll-bars . nil) default-frame-alist)
    (setq-default mode-line-format nil)

    ;; Enable font-lock and transient-mark modes to allow theme loading in batch mode
    (global-font-lock-mode +1)
    (transient-mark-mode +1)

    ;; Use system clipboard through OSC 52
    (require 'clipetty)
    (global-clipetty-mode +1)

    ${lib.optionalString evil ''
      (require 'evil)
      (evil-mode +1)
      (require 'evil-collection)
      (evil-collection-init '(magit vertico))
    ''}

    ;; Enable Vertico for better completion
    (require 'vertico)
    (vertico-mode +1)

    ;; Load Magit
    (require 'magit)
    (defun magit-mode-quit-window-or-kill-emacs (kill-buffer)
      (if (derived-mode-p 'magit-status-mode)
          (run-at-time 0 nil #'kill-emacs)
        (magit-mode-quit-window kill-buffer)))
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
          magit-bury-buffer-function    #'magit-mode-quit-window-or-kill-emacs)

    ;; Load theme
    (require 'doom-themes)
    (load-theme '${theme} t)
  '';

  runtimeConfig = writeText "init.el" ''
    (setq temporary-file-directory
          (let ((tempdir (or (getenv "TMPDIR")
                             (if (eq system-type 'windows-nt) (getenv "TEMP"))
                             "/tmp")))
            (file-name-as-directory tempdir)))

    (add-hook 'emacs-startup-hook #'magit-status)
  '';
in
assert lib.assertMsg
  (builtins.pathExists "${emacsPackages.doom-themes}/share/emacs/site-lisp/elpa/doom-themes-${emacsPackages.doom-themes.version}/${theme}-theme.el")
  "Invalid theme '${theme}'";
stdenv.mkDerivation {
  pname = if evil then "evil-magit-standalone" else "magit-standalone";
  version = "${emacsPackages.magit.version}-1";

  dontUnpack = true;

  nativeBuildInputs = [
    emacsWithPackages
    makeWrapper
  ];
  buildInputs = [ git ];

  buildPhase = ''
    ${emacsWithPackages}/bin/emacs --batch \
      -q \
      -nl \
      --no-site-file \
      -l ${configToDump} \
      --eval '(dump-emacs-portable "emacs.pdmp")'
  '';

  installPhase = ''
    mkdir -p $out/bin $out/share/magit-standalone

    cp emacs.pdmp $out/share/magit-standalone/emacs.pdmp

    makeWrapper ${emacsWithPackages}/bin/emacs $out/bin/magit \
      --add-flags "--dump-file=$out/share/magit-standalone/emacs.pdmp" \
      --add-flags '-q' \
      --add-flags '-nl' \
      --add-flags '--no-site-file' \
      --add-flags '-l ${runtimeConfig}'
  '';

  meta = with lib; {
    description = "Standalone Magit ${lib.optionalString evil " with Evil support"}";
    homepage = "https://github.com/sei40kr/nix-magit";
    license = licenses.gpl3Plus;
    platforms = platforms.unix;
    mainProgram = "magit";
  };
}
