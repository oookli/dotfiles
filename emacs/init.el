;;; -*- lexical-binding: t -*-

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        package-enable-at-startup nil)

  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t))

  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))


(eval-when-compile
  (require 'package)
  ;; tells emacs not to load any packages before starting up
  ;; the following lines tell emacs where on the internet to look up
  ;; for new packages.
  (setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           ("elpa"         . "https://elpa.gnu.org/packages/")
                           ("repo-org"     . "https://orgmode.org/elpa/")
                           ("org"          . "http://orgmode.org/elpa/")
                           ("marmalade"    . "http://marmalade-repo.org/packages/")
                           ("elpy"         . "https://jorgenschaefer.github.io/packages/")))
  ;; (package-initialize)
  (unless package--initialized (package-initialize t))

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package) ; unless it is already installed
    (package-refresh-contents) ; updage packages archive
    (package-install 'use-package)) ; and install the most recent version of use-package

  (require 'use-package)
  (setq use-package-always-ensure t))

(setq mac-right-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-left-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq mac-command-modifier 'super)

(setq mac-right-option-modifier 'nil)

(set-face-background 'show-paren-match "grey84")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(show-paren-mode)

;; (global-visual-line-mode 1)
(setq org-startup-truncated nil)

(setq column-number-mode t) ;; show columns in addition to rows in mode line

(setq-default frame-title-format "%b (%f)")

(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq-default c-basic-offset 2)
(setq c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

(blink-cursor-mode 0)

(global-visual-line-mode t)

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq
    evil-want-C-u-scroll t
    ;; don't move back the cursor one position when exiting insert mode
    evil-move-cursor-back nil)
  :config
    (setq doc-view-continuous t)
