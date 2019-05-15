;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-
;;;

;;; Functions & Macros

(defmacro add-hook! (hook &rest body)
  "Nicer add-hooking that prevents writing lambdas explicitely.
Add a lamdba containing BODY to hook HOOK."
  (declare (indent 1))
  `(add-hook ,hook
             (lambda () ,@body)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Package.el

(defun sam--increase-gc-threshold! ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun sam--normal-gc-threshold! ()
  (setq gc-cons-threshold 800000))

(add-hook! 'minibuffer-setup-hook
  (sam--increase-gc-threshold!))

(add-hook! 'minibuffer-exit-hook
  (sam--normal-gc-threshold!))

(sam--increase-gc-threshold!)
(add-hook! 'after-init-hook
  ;; restore after startup
  (sam--normal-gc-threshold!))

(setq package-enable-at-startup nil)
(let ((default-directory "~/.emacs.d/elpa"))
   (normal-top-level-add-subdirs-to-load-path))
(package-initialize t)
;; (require 'use-package)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package package
  :config
  (setq package-check-signature nil)
  (setq package-enable-at-startup nil)
  (setq use-package-verbose nil)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("elpy" . "https://jorgenschaefer.github.io/packages/"))))

(use-package general :ensure t)
; (use-package diminish)
(use-package bind-key)

;;; Sane default

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq
    delete-old-versions -1
    version-control t
    vc-make-backup-files t
    vc-follow-symlinks t
    auto-save-file-name-transforms
    '((".*" "~/.emacs.d/auto-save-list/" t))
    inhibit-startup-screen t
    ring-bell-function 'ignore
    sentence-end-double-space nil
    default-fill-column 80
    initial-scratch-message ""
    save-interprogram-paste-before-kill t
    help-window-select t
    tab-width 2
    auto-window-vscroll nil

    inhibit-splash-screen t
    inhibit-startup-message t
    inhibit-startup-echo-area-message t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default indent-tabs-mode nil
              tab-width 2)

(prefer-coding-system 'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p) ; remplace yes no par y n

(show-paren-mode) ; highlight delimiters
(line-number-mode -1) ; display line number in mode line
(column-number-mode -1) ; display colum number in mode line
(save-place-mode)    ; save cursor position between sessions
(delete-selection-mode 1)               ; replace highlighted text with type
(setq initial-major-mode 'fundamental-mode)

;; delete spacing when save
(add-hook! 'before-save-hook
  (delete-trailing-whitespace))

;; make file executable
(add-hook! 'after-save-hook
  (executable-make-buffer-file-executable-if-script-p))

(defvar my-font-for-light "DejaVu Sans Mono 12")

(when window-system
  (setq-default line-spacing 6) ; increase between-line space.

  ;; change default font for current frame
  (add-to-list 'default-frame-alist `(font . ,my-font-for-light))
  (set-face-attribute 'default nil :font my-font-for-light))

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

; (defun my--set-initial-frame ()
;   "Set the default dimension and position of a new frame."
;   (let* ((a-width (* (display-pixel-width) 0.50))
;          (a-height (* (display-pixel-height) 0.60))
;          (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
;          (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))

;     (set-frame-position (selected-frame) a-left a-top)
;     (set-frame-size (selected-frame)
;                     (truncate a-width)
;                     (truncate a-height)
;                     t )))

; (my--set-initial-frame)

; (defvar my--window-parameters
;   '(window-parameters . ((no-other-window . t)
;                          (no-delete-other-windows . t))))

; (setq
;     display-buffer-alist
;     `(("\\*Buffer List\\*" display-buffer-in-side-window
;         (side . top)
;         (slot . -1)
;         (window-height . 20)
;         (preserve-size . (nil . t)) ,my--window-parameters)
;     ("\\*Tags List\\*" display-buffer-in-side-window
;         (side . right)
;         (slot . 1)
;         (window-width . fit-window-to-buffer)
;         (preserve-size . (t . nil)) ,my--window-parameters)
;     ("\\*\\(?:help\\|grep\\|Completions\\)\\*"
;         display-buffer-in-side-window
;         (side . bottom)
;         (slot . -1)
;         (preserve-size . (nil . t))
;         ,my--window-parameters)
;     ("\\*\\(?:shell\\|compilation\\)\\*" display-buffer-in-side-window
;         (side . bottom)
;         (slot . 1)
;         (preserve-size . (nil . t))
;         ,my--window-parameters)))
    ;; ("\\*Org Select\\*" display-buffer-in-side-window
    ;;     (side . top)
    ;;     (slot . -1)
    ;;     (window-width . fit-window-to-buffer)
    ;;     (preserve-size . (t . nil))
    ;;     ,parameters)))

;;; keybindings

(when (eq system-type 'darwin)           ; mac specific bindings
  ;; (setq mac-right-command-modifier 'meta ; cmd de droite = meta
  ;;       mac-command-modifier 'control    ; cmd de gauche = control
  ;;       mac-option-modifier 'super       ; option de gauche = super
  ;;       mac-right-option-modifier nil ; option de droite = carac spéciaux
  ;;       mac-control-modifier 'hyper ; control de gauche = hyper (so does capslock)
  ;;       ns-function-modifier 'hyper ; fn key = hyper
  ;;       ns-right-alternate-modifier nil) ; cette touche n'existe pas.
  (setq mac-pass-command-to-system nil) ; disable system call to commands like
                                        ; C-h (hide frame on macOS by default
  (setq mac-pass-control-to-system nil) ; idem
  (setq-default locate-command "mdfind")
  (setq-default cursor-type 'box)

  (setq delete-by-moving-to-trash t)
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash") nil nil nil file)))

;; Essential settings.
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)

(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
;; (column-number-mode t)

;; Allow confusing functions
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;;; File type overrides.
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))

;;; My own configurations, which are bundled in my dotfiles.
;; (require 'init-platform)
(require 'init-global-functions)

;;; Larger package-specific configurations.
;; (require 'init-fonts)
;; (require 'init-gtags)
(require 'init-evil)
;; (require 'init-maps)
;; (require 'init-flycheck)
;; (require 'init-tmux)

(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")

;; Utilities
; (use-package s
;   :ensure t
;   :defer 1)
; (use-package dash :ensure t)

; (use-package visual-fill-column :ensure t)

; (use-package all-the-icons
;   :ensure t)

; (use-package all-the-icons-dired
;   :ensure t)

;; (use-package helm-make
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-c m") 'helm-make-projectile))

;; (use-package dired
;;   :config
;;   (require 'dired-x)
;;   (setq dired-omit-files "^\\.?#\\|^\\.[^.].*")

;;   (defun air-dired-buffer-dir-or-home ()
;;     "Open dired to the current buffer's dir, or $HOME."
;;     (interactive)
;;     (let ((cwd (or (file-name-directory (or (buffer-file-name) ""))
;;                    (expand-file-name "~"))))
;;       (dired cwd)))

;;   (add-hook 'dired-mode-hook (lambda ()
;;                                (dired-omit-mode t)
;;                                (all-the-icons-dired-mode t)))
;;   (define-key dired-mode-map (kbd "RET")     'dired-find-alternate-file)
;;   (define-key dired-mode-map (kbd "^")       (lambda () (interactive) (find-alternate-file "..")))
;;   (define-key dired-mode-map (kbd "C-.")     'dired-omit-mode)
;;   (define-key dired-mode-map (kbd "c")       'find-file)
;;   (define-key dired-mode-map (kbd "/")       'counsel-grep-or-swiper)
;;   (define-key dired-mode-map (kbd "?")       'evil-search-backward)
;;   (define-key dired-mode-map (kbd "C-c C-c") 'dired-toggle-read-only))

;; (eval-after-load 'wdired
;;   (add-hook 'wdired-mode-hook 'evil-normal-state))

; (use-package rainbow-mode
;   :ensure t
;   :commands rainbow-mode)

; (use-package css-mode
;   :ensure t
;   :config
;   (add-hook 'css-mode-hook (lambda ()
;                              (rainbow-mode))))

;; (use-package wgrep
;;   :ensure t
;;   :config
;;   (setq wgrep-auto-save-buffer t)
;;   (defadvice wgrep-change-to-wgrep-mode (after wgrep-set-normal-state)
;;     (if (fboundp 'evil-normal-state)
;;         (evil-normal-state)))
;;   (ad-activate 'wgrep-change-to-wgrep-mode)

;;   (defadvice wgrep-finish-edit (after wgrep-set-motion-state)
;;     (if (fboundp 'evil-motion-state)
;;         (evil-motion-state)))
;;   (ad-activate 'wgrep-finish-edit))

;; (use-package wgrep-ag
;;   :ensure t
;;   :commands (wgrep-ag-setup))

;; (use-package ag
;;   :ensure t
;;   :commands (ag ag-project)
;;   :config
;;   (add-hook 'ag-mode-hook
;;             (lambda ()

;;               (define-key ag-mode-map (kbd "n") 'evil-search-next)
;;               (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
;;   (setq ag-executable "/usr/local/bin/ag")
;;   (setq ag-highlight-search t)
;;   (setq ag-reuse-buffers t)
;;   (setq ag-reuse-window t))

(use-package js2-mode
  :ensure t
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package color-theme-sanityinc-tomorrow
    :ensure t
		:config
		(load-theme 'sanityinc-tomorrow-night t))

;; (use-package company
;;   :ensure t
;;   :defer t
;;   :init
;;   (global-company-mode)
;;   :config

;;   (defun org-keyword-backend (command &optional arg &rest ignored)
;;     "Company backend for org keywords.
;; COMMAND, ARG, IGNORED are the arguments required by the variable
;; `company-backends', which see."
;;     (interactive (list 'interactive))
;;     (cl-case command
;;       (interactive (company-begin-backend 'org-keyword-backend))
;;       (prefix (and (eq major-mode 'org-mode)
;;                    (let ((p (company-grab-line "^#\\+\\(\\w*\\)" 1)))
;;                      (if p (cons p t)))))
;;       (candidates (mapcar #'upcase
;;                           (cl-remove-if-not
;;                            (lambda (c) (string-prefix-p arg c))
;;                            (pcomplete-completions))))
;;       (ignore-case t)
;;       (duplicates t)))
;;   (add-to-list 'company-backends 'org-keyword-backend)

;;   (setq company-idle-delay 0.4)
;;   (setq company-selection-wrap-around t)
;;   (define-key company-active-map (kbd "ESC") 'company-abort)
;;   (define-key company-active-map [tab] 'company-complete-common-or-cycle)
;;   (define-key company-active-map (kbd "C-n") 'company-select-next)
;;   (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; (use-package counsel :ensure t)

;; (use-package swiper
;;   :ensure t
;;   :commands swiper
;;   :bind ("C-s" . counsel-grep-or-swiper)
;;   :config
;;   (require 'counsel)
;;   (setq counsel-grep-base-command "grep -niE \"%s\" %s")
;;   (setq ivy-height 20))

;; (use-package dictionary :ensure t)

;; (use-package emmet-mode
;;   :ensure t
;;   :commands emmet-mode)

;; (use-package flycheck
;;   :ensure t
;;   :commands flycheck-mode)

;; (use-package helm-projectile
;;   :commands (helm-projectile helm-projectile-switch-project)
;;   :ensure t)

;; (use-package web-mode
;;   :ensure t
;;   :defer t)

;; (use-package mmm-mode :ensure t :defer t)
;; (use-package yaml-mode :ensure t :defer t)

(use-package which-key
  :ensure t
  :diminish ""
  :config
  (which-key-mode t))

(use-package projectile
  :ensure t
  :defer 1
  :config
  (projectile-mode)
  (setq projectile-project-search-path '("~/Sites/"))
  (setq projectile-enable-caching t)
  (setq projectile-mode-line
        '(:eval
          (format " Proj[%s]"
                  (projectile-project-name)))))

(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.5))

;; (use-package magit
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq magit-branch-arguments nil)
;;   (setq magit-push-always-verify nil)
;;   (setq magit-last-seen-setup-instructions "1.4.0")
;;   (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent"))

; (require 'periodic-commit-minor-mode)

;; (use-package mmm-mode
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq mmm-global-mode 'maybe)
;;   (mmm-add-classes
;;    '((markdown-cl
;;       :submode emacs-lisp-mode
;;       :face mmm-declaration-submode-face
;;       :front "^~~~cl[\n\r]+"
;;       :back "^~~~$")
;;      (markdown-php
;;       :submode php-mode
;;       :face mmm-declaration-submode-face
;;       :front "^```php[\n\r]+"
;;       :back "^```$")))
;;   (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-cl)
;;   (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-php))

;; (use-package undo-tree
;;   :ensure t
;;   :diminish t
;;   :config
;;   (setq undo-tree-auto-save-history t)
;;   (setq undo-tree-history-directory-alist
;;         (list (cons "." (expand-file-name "undo-tree-history" user-emacs-directory)))))

;;; Flycheck mode:
;; (add-hook 'flycheck-mode-hook
;;           (lambda ()
;;             (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
;;             (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; All programming modes
;; (defun air--set-up-prog-mode ()
;;   "Configure global `prog-mode'."
;;   (setq-local comment-auto-fill-only-comments t)
;;   (electric-pair-local-mode))
;; (add-hook 'prog-mode-hook 'air--set-up-prog-mode)

;;; If `display-line-numbers-mode' is available (only in Emacs 26),
;;; use it! Otherwise, install and run nlinum-relative.
(if (functionp 'display-line-numbers-mode)
    (and (add-hook 'display-line-numbers-mode-hook
                   (lambda () (setq display-line-numbers-type 'relative)))
         (add-hook 'prog-mode-hook #'display-line-numbers-mode))
		(use-package nlinum-relative
		 :ensure t
		 :config
		 (nlinum-relative-setup-evil)
		 (setq nlinum-relative-redisplay-delay 0)
		 (add-hook 'prog-mode-hook #'nlinum-relative-mode)))

(use-package esh-autosuggest
  :hook (eshell-mode-hook . esh-autosuggest-mode)
  :ensure t)

(defun air--get-vc-root ()
    "Get the root directory of the current VC project.
This function assumes that the current buffer is visiting a file that
is within a version controlled project."
    (require 'vc)
    (vc-call-backend
     (vc-responsible-backend (buffer-file-name))
     'root (buffer-file-name)))

;;; The Emacs Shell
;; (defun company-eshell-history (command &optional arg &rest ignored)
;;   "Complete from shell history when starting a new line.
;; Provide COMMAND and ARG in keeping with the Company Mode backend spec.
;; The IGNORED argument is... Ignored."
;;   (interactive (list 'interactive))
;;   (cl-case command
;;     (interactive (company-begin-backend 'company-eshell-history))
;;     (prefix (and (eq major-mode 'eshell-mode)
;;                  (let ((word (company-grab-word)))
;;                    (save-excursion
;;                      (eshell-bol)
;;                      (and (looking-at-p (s-concat word "$")) word)))))
;;     (candidates (remove-duplicates
;;                  (->> (ring-elements eshell-history-ring)
;;                       (remove-if-not (lambda (item) (s-prefix-p arg item)))
;;                       (mapcar 's-trim))
;;                  :test 'string=))
;;     (sorted t)))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill term buffer when term is ended."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Eshell things
(defun my--eshell-clear ()
  "Clear an eshell buffer and re-display the prompt."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; (eshell-send-input)
    ))

(defun my--eshell-mode-hook ()
  "Eshell mode settings."
  (define-key eshell-mode-map (kbd "C-u") 'eshell-kill-input)
  (define-key eshell-mode-map (kbd "C-l") 'my--eshell-clear)
  (define-key eshell-mode-map (kbd "C-d") (lambda () (interactive)
                                            (kill-this-buffer)
                                            (if (not (one-window-p))
                                                (delete-window))))
  (set (make-local-variable 'pcomplete-ignore-case) t)
  (set (make-local-variable 'company-backends)
       '((esh-autosuggest))))

(add-hook 'eshell-mode-hook 'my--eshell-mode-hook)

;;; Magit mode (which does not open in evil-mode):
;; (add-hook 'magit-mode-hook
;;           (lambda ()
;;             (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

;;; Git Commit Mode (a Magit minor mode):
;; (add-hook 'git-commit-mode-hook 'evil-insert-state)

;;; Emmet mode:
;; (add-hook 'emmet-mode-hook
;;           (lambda ()
;;             (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
;;             (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point)))

;;; Web mode:
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (setq web-mode-style-padding 2)
;;             (yas-minor-mode t)
;;             (emmet-mode)
;;             (flycheck-add-mode 'html-tidy 'web-mode)
;;             (flycheck-mode)))

;; (setq web-mode-ac-sources-alist
;;       '(("php" . (ac-source-php-extras ac-source-yasnippet ac-source-gtags ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
;;         ("css" . (ac-source-css-property ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))))

;; (add-hook 'web-mode-before-auto-complete-hooks
;;           '(lambda ()
;;              (let ((web-mode-cur-language (web-mode-language-at-pos)))
;;                (if (string= web-mode-cur-language "php")
;;                    (yas-activate-extra-mode 'php-mode)
;;                  (yas-deactivate-extra-mode 'php-mode))
;;                (if (string= web-mode-cur-language "css")
;;                    (setq emmet-use-css-transform t)
;;                  (setq emmet-use-css-transform nil)))))

;;; Emacs Lisp mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; (yas-minor-mode t)
            (eldoc-mode)
            (highlight-symbol-mode)
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; SH mode:
;; (add-hook 'sh-mode-hook (lambda ()
;;                           (setq sh-basic-offset 2)
;;                           (setq sh-indentation 2)))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  (sml/setup))

(server-start)

;; (defun setup-eshell-helm-completion ()
;;   (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete))

;; (use-package helm
;;   :ensure t
;;   :diminish helm-mode
;;   :commands helm-mode
;;   :config
;;   (helm-mode 1)
;;   (add-hook 'eshell-mode-hook #'setup-eshell-helm-completion)
;;   (setq helm-buffers-fuzzy-matching t)
;;   (setq helm-autoresize-mode t)
;;   (setq helm-buffer-max-length 40)
;;   (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
;;   (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)
;;   (define-key helm-map (kbd "C-j") 'helm-next-line)
;;   (define-key helm-map (kbd "C-k") 'helm-previous-line)
;;   (define-key helm-map (kbd "C-h") 'helm-next-source)
;;   (define-key helm-map (kbd "C-S-h") 'describe-key)
;;   (define-key helm-map (kbd "C-l") (kbd "RET"))
;;   (define-key helm-map [escape] 'helm-keyboard-quit)
;;   (define-key helm-map (kbd "C-SPC") 'helm-keyboard-quit)
;;   (dolist
;;       (keymap (list helm-find-files-map helm-read-file-map))
;;       (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
;;       (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
;;       (define-key keymap (kbd "C-j") 'helm-next-line)
;;       (define-key keymap (kbd "C-k") 'helm-previous-line)
;;       (define-key keymap (kbd "C-S-h") 'describe-key)
;;       (define-key keymap (kbd "C-SPC") 'helm-keyboard-quit)
;;   )
;; )

;; (use-package helm-ag
;;   :ensure t)

;; (defun spacemacs//hide-cursor-in-helm-buffer ()
;;   "Hide the cursor in helm buffers."
;;   (with-helm-buffer
;;     (setq cursor-in-non-selected-windows nil)))
;; (add-hook 'helm-after-initialize-hook
;;           'spacemacs//hide-cursor-in-helm-buffer)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (defun rjsx-mode-config ()
    "Configure RJSX Mode"
    (define-key rjsx-mode-map (kbd "C-j") 'rjsx-delete-creates-full-tag))
  (add-hook 'rjsx-mode-hook 'rjsx-mode-config))

(use-package slim-mode :ensure t)

;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;; used in OS X. Disable sRGB before setting up Powerline.
;; (when (memq window-system '(mac ns))
;;   (setq ns-use-srgb-colorspace nil))

;; (defface my-pl-segment1-active
;;   '((t (:foreground "#000000" :background "#E1B61A")))
;;   "Powerline first segment active face.")
;; (defface my-pl-segment1-inactive
;;   '((t (:foreground "#CEBFF3" :background "#3A2E58")))
;;   "Powerline first segment inactive face.")
;; (defface my-pl-segment2-active
;;   '((t (:foreground "#F5E39F" :background "#8A7119")))
;;   "Powerline second segment active face.")
;; (defface my-pl-segment2-inactive
;;   '((t (:foreground "#CEBFF3" :background "#3A2E58")))
;;   "Powerline second segment inactive face.")
;; (defface my-pl-segment3-active
;;   '((t (:foreground "#CEBFF3" :background "#3A2E58")))
;;   "Powerline third segment active face.")
;; (defface my-pl-segment3-inactive
;;   '((t (:foreground "#CEBFF3" :background "#3A2E58")))
;;   "Powerline third segment inactive face.")

;; (defun air--powerline-default-theme ()
;;   "Set up my custom Powerline with Evil indicators."
;;   (setq-default mode-line-format
;;                 '("%e"
;;                   (:eval
;;                    (let* ((active (powerline-selected-window-active))
;;                           (seg1 (if active 'my-pl-segment1-active 'my-pl-segment1-inactive))
;;                           (seg2 (if active 'my-pl-segment2-active 'my-pl-segment2-inactive))
;;                           (seg3 (if active 'my-pl-segment3-active 'my-pl-segment3-inactive))
;;                           (separator-left (intern (format "powerline-%s-%s"
;;                                                           (powerline-current-separator)
;;                                                           (car powerline-default-separator-dir))))
;;                           (separator-right (intern (format "powerline-%s-%s"
;;                                                            (powerline-current-separator)
;;                                                            (cdr powerline-default-separator-dir))))
;;                           (lhs (list (let ((evil-face (powerline-evil-face)))
;;                                        (if evil-mode
;;                                            (powerline-raw (powerline-evil-tag) evil-face)
;;                                          ))
;;                                      (if evil-mode
;;                                          (funcall separator-left (powerline-evil-face) seg1))
;;                                      (powerline-buffer-id seg1 'l)
;;                                      (powerline-raw "[%*]" seg1 'l)
;;                                      (when (and (boundp 'which-func-mode) which-func-mode)
;;                                        (powerline-raw which-func-format seg1 'l))
;;                                      (powerline-raw " " seg1)
;;                                      (funcall separator-left seg1 seg2)
;;                                      (when (boundp 'erc-modified-channels-object)
;;                                        (powerline-raw erc-modified-channels-object seg2 'l))
;;                                      (powerline-major-mode seg2 'l)
;;                                      (powerline-process seg2)
;;                                      (powerline-minor-modes seg2 'l)
;;                                      (powerline-narrow seg2 'l)
;;                                      (powerline-raw " " seg2)
;;                                      (funcall separator-left seg2 seg3)
;;                                      (powerline-vc seg3 'r)
;;                                      (when (bound-and-true-p nyan-mode)
;;                                        (powerline-raw (list (nyan-create)) seg3 'l))))
;;                           (rhs (list (powerline-raw global-mode-string seg3 'r)
;;                                      (funcall separator-right seg3 seg2)
;;                                      (unless window-system
;;                                        (powerline-raw (char-to-string #xe0a1) seg2 'l))
;;                                      (powerline-raw "%4l" seg2 'l)
;;                                      (powerline-raw ":" seg2 'l)
;;                                      (powerline-raw "%3c" seg2 'r)
;;                                      (funcall separator-right seg2 seg1)
;;                                      (powerline-raw " " seg1)
;;                                      (powerline-raw "%6p" seg1 'r)
;;                                      (when powerline-display-hud
;;                                        (powerline-hud seg1 seg3)))))
;;                      (concat (powerline-render lhs)
;;                              (powerline-fill seg3 (powerline-width rhs))
;;                              (powerline-render rhs)))))))

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (setq powerline-default-separator (if (display-graphic-p) 'arrow
;;                                       nil))
;;   (air--powerline-default-theme))

;; (use-package powerline-evil
;;   :ensure t)
(setq-default tab-width 2)


;; (use-package wgrep
;;   :ensure t
;;   :config
;;   (setq wgrep-auto-save-buffer t)
;;   (defadvice wgrep-change-to-wgrep-mode (after wgrep-set-normal-state)
;;     (if (fboundp 'evil-normal-state)
;;         (evil-normal-state)))
;;   (ad-activate 'wgrep-change-to-wgrep-mode)

;;   (defadvice wgrep-finish-edit (after wgrep-set-motion-state)
;;     (if (fboundp 'evil-motion-state)
;;         (evil-motion-state)))
;;   (ad-activate 'wgrep-finish-edit))

;; (use-package wgrep-ag
;;   :ensure t
;;   :commands (wgrep-ag-setup))

;; (use-package ag
;;   :ensure t
;;   :commands (ag ag-project)
;;   :config
;;   (add-hook 'ag-mode-hook
;;             (lambda ()
;;               (wgrep-ag-setup)
;;               (evil-define-key ag-mode-map (kbd "n") 'evil-search-next)
;;               (evil-define-key ag-mode-map (kbd "N") 'evil-search-previous)))
;;   (setq ag-executable "/usr/local/bin/ag")
;;   (setq ag-highlight-search t)
;;   (setq ag-reuse-buffers t)
;;   (setq ag-reuse-window t))

(electric-pair-mode 1)
(electric-indent-mode 1)
(setq-default electric-indent-inhibit t)

;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pb-copy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)

(fset 'evil-visual-update-x-selection 'ignore)

; Highlight tabs and trailing whitespace everywhere
(setq whitespace-style '(face trailing tabs))
(custom-set-faces
 '(whitespace-tab ((t (:background unspecified :foreground "gray30" :weight bold)))))
(standard-display-ascii ?\t ">·")
(global-whitespace-mode)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode t))

(require 'init-identation)

;; (use-package ace-window
;;     :ensure t
;;     :config
;;     (setq aw-dispatch-always t)
;; :bind(("M-o" . ace-window)))

;; use Emacs keybindings when in insert mode }:)
;; (setcdr evil-insert-state-map nil)
;; (define-key evil-insert-state-map [escape] 'evil-normal-state)

(savehist-mode 1)

(use-package fill-column-indicator
  :ensure t
  :init
  (setq
   fci-rule-width 5
   fci-rule-color "#333333")
  :config
  (dolist (hook '(rjsx-mode-hook js2-mode-hook web-mode-hook))
    (add-hook hook (lambda ()
                                (turn-on-auto-fill)
                                (fci-mode)))
    )
  )

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("#cc6666" box))
(setq evil-visual-state-cursor '("#cc6666" box))
(setq evil-insert-state-cursor '("#cc6666" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(provide 'init)
;;; init.el ends here
