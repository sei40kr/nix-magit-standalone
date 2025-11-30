{
  lib,
  stdenv,
  emacsPackages,
  emacs,
  makeWrapper,
  nano,
  writeText,
  writeShellScript,
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
      epkgs.vterm
    ]
    ++ lib.optionals evil [
      epkgs.evil
      epkgs.evil-collection
    ]
  );

  # Custom emacsclient wrapper that opens files in external editor via vterm
  vterm-external-editor-launcher = writeShellScript "vterm-external-editor-launcher" ''
    file=""
    options=()

    while [ $# -gt 0 ]; do
      case "$1" in
        -s|--socket-name|-a|--alternate-editor)
          options+=("$1")
          shift
          if [ $# -gt 0 ]; then
            options+=("$1")
            shift
          fi
          ;;
        -*)
          options+=("$1")
          shift
          ;;
        *)
          file="$1"
          shift
          ;;
      esac
    done

    if [ -n "$file" ]; then
      exec ${emacsWithPackages}/bin/emacsclient -q "''${options[@]}" --eval "(magit-standalone--open-in-external-editor \"$file\")"
    fi
  '';

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

    (defun magit-standalone--open-in-external-editor (file)
      "Open FILE in external editor using vterm in fullscreen."
      (let* ((editor-cmd (or (getenv "GIT_EDITOR")
                             (getenv "EDITOR")
                             "${nano}/bin/nano"))
             (editor-args (append (split-string-and-unquote editor-cmd) (list file)))
             (buffer-name (format "*Git Editor: %s*" (file-name-nondirectory file)))
             ;; Create temporary shell script to launch editor directly
             (editor-launch-script (make-temp-file "editor-launch-" nil ".sh")))

        ;; Kill existing buffer if present
        (when-let ((existing-buffer (get-buffer buffer-name)))
          (kill-buffer existing-buffer))

        ;; Write shell script that execs the editor
        (with-temp-file editor-launch-script
          (insert "#!/bin/sh\nexec")
          (dolist (arg editor-args)
            (insert " " (shell-quote-argument arg))))
        (set-file-modes editor-launch-script #o700)

        ;; Save current window configuration before opening vterm
        (let* ((saved-window-config (current-window-configuration))
               (vterm-shell editor-launch-script))
          (unwind-protect
              (progn
                (switch-to-buffer (vterm buffer-name))
                (delete-other-windows)

                ${lib.optionalString evil ''
                  ;; Disable Evil mode in vterm to allow all keys to pass through
                  (when (fboundp 'evil-local-mode)
                    (evil-local-mode -1))
                ''}

                ;; Set up exit hook to close buffer and restore window configuration
                (add-hook 'vterm-exit-functions
                          (lambda (buffer _event)
                            (when (buffer-live-p buffer)
                              (kill-buffer buffer))
                            ;; Restore window configuration
                            (set-window-configuration saved-window-config)
                            ;; Exit recursive-edit when process finishes
                            (exit-recursive-edit))
                          nil t)

                ;; Enter recursive-edit to wait for process while allowing user input
                (recursive-edit))
            ;; Clean-up: Always delete the temporary script file
            (when (file-exists-p editor-launch-script)
              (delete-file editor-launch-script))))))

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
    (setq magit-display-buffer-function      #'magit-display-buffer-fullframe-status-v1
          magit-bury-buffer-function         #'magit-mode-quit-window-or-kill-emacs
          with-editor-emacsclient-executable "${vterm-external-editor-launcher}")

    ;; Load theme
    (require 'doom-themes)
    (load-theme '${theme} t)
  '';

  runtimeConfig = writeText "init.el" ''
    ;; Override directories at runtime.
    ;; The portable dumper captures paths from the isolated Nix build environment,
    ;; so we need to reconfigure them based on the actual runtime environment.
    (setq temporary-file-directory
          (let ((tempdir (or (getenv "TMPDIR")
                             (if (eq system-type 'windows-nt) (getenv "TEMP"))
                             "/tmp")))
            (file-name-as-directory tempdir)))
    (setq server-socket-dir
          (concat temporary-file-directory
                  "emacs"
                  (if (fboundp 'user-uid) (format "%d" (user-uid)) "")))

    ;; Pass all keys to the terminal (disable Emacs keybindings in vterm)
    (setq vterm-keymap-exceptions '("C-c"))
    ;; Load vterm at runtime instead of dump time.
    ;; vterm uses dynamic modules (native code) which cannot be serialized
    ;; in the portable dump due to "unsupported object type in dump: pseudovector type 25".
    (require 'vterm)

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
  buildInputs = [
    git
    nano
  ];

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
      --argv0 magit-standalone \
      --add-flags "--dump-file=$out/share/magit-standalone/emacs.pdmp" \
      --add-flags '-q' \
      --add-flags '-nl' \
      --add-flags '--no-site-file' \
      --add-flags '-l ${runtimeConfig}'
  '';

  meta = with lib; {
    description = "Standalone Magit ${lib.optionalString evil " with Evil support"}";
    homepage = "https://github.com/sei40kr/nix-magit-standalone";
    license = licenses.gpl3Plus;
    platforms = platforms.unix;
    mainProgram = "magit";
  };
}
