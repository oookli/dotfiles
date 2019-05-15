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

(defun my--increase-gc-threshold! ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my--normal-gc-threshold! ()
  (setq gc-cons-threshold 800000))

(add-hook! 'minibuffer-setup-hook
  (my--increase-gc-threshold!))

(add-hook! 'minibuffer-exit-hook
  (my--normal-gc-threshold!))

(my--increase-gc-threshold!)
(add-hook! 'after-init-hook
  ;; restore after startup
  (my--normal-gc-threshold!))

(require 'init-elpa)

(setq package-enable-at-startup nil)
(let ((default-directory "~/.emacs.d/elpa"))
   (normal-top-level-add-subdirs-to-load-path))
(package-initialize t)

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
  (setq use-package-verbose nil))

;; (use-package general :ensure t)
;; (use-package diminish)
;; (use-package bind-key)

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

(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package which-key
  :ensure t
  :config (which-key-mode t))

(use-package general
  :after which-key
  :config
  (general-override-mode 1)

  (defun find-user-init-file ()
    "Edit the `user-init-file', in same window."
    (interactive)
    (find-file user-init-file))
  (defun load-user-init-file ()
    "Load the `user-init-file', in same window."
    (interactive)
    (load-file user-init-file))

  ;;Taken from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
  (defun rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (message "Buffer is not visiting a file!")
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))


  (defun disable-all-themes ()
    "disable all active themes."
    (dolist (i custom-enabled-themes)
      (disable-theme i)))

  (defadvice load-theme (before disable-themes-first activate)
    (disable-all-themes))

  ;; Following lines to cycle through themes adapted from ivan's answer on
  ;; https://emacs.stackexchange.com/questions/24088/make-a-function-to-toggle-themes
  (setq my/themes (custom-available-themes))
  (setq my/themes-index 0)

  (defun my/cycle-theme ()
    "Cycles through my themes."
    (interactive)
    (setq my/themes-index (% (1+ my/themes-index) (length my/themes)))
    (my/load-indexed-theme))

  (defun my/load-indexed-theme ()
    (load-theme (nth my/themes-index my/themes)))

  (defun load-leuven-theme ()
    "Loads `leuven' theme"
    (interactive)
    (load-theme 'leuven))

  (defun load-dichromacy-theme ()
    "Loads `dichromacy' theme"
    (interactive)
    (load-theme 'dichromacy))

  (general-create-definer tyrant-def
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer despot-def
    :states '(normal insert)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-define-key
    :keymaps 'key-translation-map
    "ESC" (kbd "C-g"))

  (general-def
    "C-x x" 'eval-defun)

  (tyrant-def

    ""     nil
    "c"   (general-simulate-key "C-c")
    "h"   (general-simulate-key "C-h")
    "u"   (general-simulate-key "C-u")
    "x"   (general-simulate-key "C-x")

    ;; Package manager
    "lp"  'list-packages

    ;; Theme operations
    "t"   '(:ignore t :which-key "themes")
    "tn"  'my/cycle-theme
    "tt"  'load-theme
    "tl"  'load-leuven-theme
    "td"  'load-dichromacy-theme

    ;; Quit operations
    "q"	  '(:ignore t :which-key "quit emacs")
    "qq"  'kill-emacs
    "qz"  'delete-frame

    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffer")
    "bb"  'mode-line-other-buffer
    "bd"  'kill-this-buffer
    "b]"  'next-buffer
    "b["  'previous-buffer
    "bq"  'kill-buffer-and-window
    "bR"  'rename-file-and-buffer
    "br"  'revert-buffer

    ;; Window operations
    "w"   '(:ignore t :which-key "window")
    "wm"  'maximize-window
    "w/"  'split-window-horizontally
    "wv"  'split-window-vertically
    "wm"  'maximize-window
    "wu"  'winner-undo
    "ww"  'other-window
    "wd"  'delete-window
    "wD"  'delete-other-windows

    ;; File operations
    "f"   '(:ignore t :which-key "files")
    "fc"  'write-file
    "fe"  '(:ignore t :which-key "emacs")
    "fed" 'find-user-init-file
    "feR" 'load-user-init-file
    "fj"  'dired-jump
    "fl"  'find-file-literally
    "fR"  'rename-file-and-buffer
    "fs"  'save-buffer

    ;; Applications
    "a"   '(:ignore t :which-key "Applications")
    "ad"  'dired
    ":"   'shell-command
    ";"   'eval-expression
    "ac"  'calendar
    "oa"  'org-agenda)

  ; (general-def 'normal doc-view-mode-map
  ;   "j"   'doc-view-next-line-or-next-page
  ;   "k"   'doc-view-previous-line-or-previous-page
  ;   "gg"  'doc-view-first-page
  ;   "G"   'doc-view-last-page
  ;   "C-d" 'doc-view-scroll-up-or-next-page
  ;   "C-f" 'doc-view-scroll-up-or-next-page
  ;   "C-b" 'doc-view-scroll-down-or-previous-page)

  (general-def '(normal visual) outline-minor-mode-map
    "zn"  'outline-next-visible-heading
    "zp"  'outline-previous-visible-heading
    "zf"  'outline-forward-same-level
    "zB"  'outline-backward-same-level)

  (general-def 'normal package-menu-mode-map
    "i"   'package-menu-mark-install
    "U"   'package-menu-mark-upgrades
    "d"   'package-menu-mark-delete
    "u"   'package-menu-mark-unmark
    "x"   'package-menu-execute
    "q"   'quit-window)

  ; (general-def 'normal calendar-mode-map
  ;   "h"   'calendar-backward-day
  ;   "j"   'calendar-forward-week
  ;   "k"   'calendar-backward-week
  ;   "l"   'calendar-forward-day
  ;   "0"   'calendar-beginning-of-week
  ;   "^"   'calendar-beginning-of-week
  ;   "$"   'calendar-end-of-week
  ;   "["   'calendar-backward-year
  ;   "]"   'calendar-forward-year
  ;   "("   'calendar-beginning-of-month
  ;   ")"   'calendar-end-of-month
  ;   "SPC" 'scroll-other-window
  ;   "S-SPC" 'scroll-other-window-down
  ;   "<delete>" 'scroll-other-window-down
  ;   "<"   'calendar-scroll-right
  ;   ">"   'calendar-scroll-left
  ;   "C-b" 'calendar-scroll-right-three-months
  ;   "C-f" 'calendar-scroll-left-three-months
  ;   "{"   'calendar-backward-month
  ;   "}"   'calendar-forward-month
  ;   "C-k" 'calendar-backward-month
  ;   "C-j" 'calendar-forward-month
  ;   "gk"  'calendar-backward-month
  ;   "gj"  'calendar-forward-month
  ;   "v"   'calendar-set-mark
  ;   "."   'calendar-goto-today
  ;   "q"   'calendar-exit))
  )

(use-package suggest
  :general (tyrant-def "as" 'suggest))

(use-package ranger
  :hook (after-init . ranger-override-dired-mode)
  :general (tyrant-def "ar" 'ranger))

(use-package solarized-theme
  :init
  (setq solarized-scale-org-headlines nil)
  (custom-set-faces '(mode-line ((t (:background "gray10"
                                     :foreground "dark gray"
                                     :box nil
                                     :weight normal
                                     :height 1.0
                                     :width normal
                                     :underline nil
                                     :overline nil))))
                    '(mode-line-inactive ((t (:background "gray10"
                                              :foreground "dark gray"
                                              :box nil
                                              :weight normal
                                              :height 1.0
                                              :width normal
                                              :underline nil
                                              :overline nil)))))
  :hook (after-init . load-solarized-dark)
  :config
  (defun load-solarized-dark ()
      "Load the `solarized-dark' theme."
      (interactive)
      (load-theme 'solarized-dark))
  (defun load-solarized-light ()
      "Load the `solarized-light' theme."
      (interactive)
      (load-theme 'solarized-light))
  :general
  (tyrant-def "ts"  '(:ignore t :which-key "solarized")
              "tsl" 'load-solarized-light
              "tsd" 'load-solarized-dark))

(use-package evil
  :hook (after-init . evil-mode)
  :config
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'doc-view-mode 'normal)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'biblio-selection-mode 'motion)
  (setq doc-view-continuous t)
  :general
  (tyrant-def
    "wh"  'evil-window-left
    "wl"  'evil-window-right
    "wj"  'evil-window-down
    "wk"  'evil-window-up
    "bN"  'evil-buffer-new
    "fd"  'evil-save-and-close)
  ('motion override-global-map
    "]b"  'evil-next-buffer
    "[b"  'evil-prev-buffer))

(use-package evil-numbers
  :after evil
  :general
  ('normal "C-=" 'evil-numbers/inc-at-pt
           "C--" 'evil-numbers/dec-at-pt))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-easymotion
  :after evil
  :config (evilem-default-keybindings "gs"))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1)
  :general
  ('normal override-global-map
    "gc"  'evil-commentary
    "gC" 'evil-commentary-line))

; (use-package evil-visualstar
;   :after evil
;   :config
;   (setq evilmi-always-simple-jump t)
;   (global-evil-visualstar-mode 1))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") #'company-select-previous-or-abort)
  (setq company-frontends '(company-echo-metadata-frontend
                            company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend))
  (setq company-backends '((company-capf
                            company-files)
                           (company-dabbrev-code company-keywords)
                            company-dabbrev company-yasnippet)))

(use-package company-quickhelp
  :defer 5
  :config (company-quickhelp-mode))

(use-package company-statistics
  :defer 5
  :config (company-statistics-mode))

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

(defvar narrowing-system "ivy"
  "Sets the narrowing system to use - helm or ivy")

(use-package ivy
    :if (equal narrowing-system "ivy")
    :hook (after-init . ivy-mode)
    :config (setq ivy-use-virtual-buffers t
                ivy-count-format "(%d/%d) "
                ivy-initial-inputs-alist nil
                ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
    :commands (ivy-switch-buffer)
    :general
    (tyrant-def "bm"  'ivy-switch-buffer))

(use-package smex
  :ensure t
  :if (equal narrowing-system "ivy"))

(use-package counsel
  :after (ivy)
  :general
  (tyrant-def
    "SPC" 'counsel-M-x
    "ff"  'counsel-find-file
    "fr"  'counsel-recentf
    "fL"  'counsel-locate))

(use-package flyspell-correct-ivy
  :if (equal narrowing-system "ivy")
  :commands (flyspell-correct-word-generic)
  :general
   (:keymaps '(flyspell-mode-map)
    :states '(normal visual)
    "zs" 'flyspell-correct-word-generic
    "z=" 'flyspell-buffer))

(use-package counsel-projectile
  :after (projectile ivy)
  :general
  (tyrant-def
   "p"   '(:ignore t :which-key "projectile")
   "pd"  'counsel-projectile-dired-find-dir
   "po"  'counsel-projectile-find-other-file
   "pf"  'counsel-projectile-find-file
   "fp"  'counsel-projectile-find-file
   "pb"  'counsel-projectile-switch-to-buffer
   "bp"  'counsel-projectile-switch-to-buffer))


(use-package helm
  :if (equal narrowing-system "helm")
  :hook (after-init . helm-mode)
  :config (require 'helm-config)
  :commands (helm-mini
             helm-find-files
             helm-recentf
             helm-locate
             helm-M-x
             helm-flyspell-correct)
  :general
  (tyrant-def
   "SPC" 'helm-M-x
   "bm"  'helm-mini
   "ff"  'helm-find-files
   "fr"  'helm-recentf
   "fL"  'helm-locate))

(use-package helm-flyspell
  :if (equal narrowing-system "helm")
  :commands (helm-flyspell-correct)
  :general
   (:keymaps '(flyspell-mode-map)
    :states '(normal visual)
    "zs" 'helm-flyspell-correct
    "z=" 'flyspell-buffer))

(use-package helm-projectile
  :after (projectile helm)
  :general
  (tyrant-def
   "p"   '(:ignore t :which-key "projectile")
   "pd"  'helm-projectile-dired-find-dir
   "po"  'helm-projectile-find-other-file
   "pf"  'helm-projectile-find-file
   "fp"  'helm-projectile-find-file
   "pb"  'helm-projectile-switch-to-buffer
   "bp"  'helm-projectile-switch-to-buffer))

(use-package flycheck
  :commands (flycheck-mode)
  :general
  (tyrant-def
   "e"   '(:ignore t :which-key "Errors")
   "en"  'flycheck-next-error
   "ep"  'flycheck-previous-error))

; (use-package magit
;   :commands (magit-status)
;   :general
;   (tyrant-def
;    "g"   '(:ignore t :which-key "git")
;    "gs"  'magit-status))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

(use-package yasnippet
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :general
  (tyrant-def
   "y"   '(:ignore t :which-key "yasnippet")
   "yi"  'yas-insert-snippet
   "yv"  'yas-visit-snippet-file
   "yn"  'yas-new-snippet))

(use-package yasnippet-snippets
  :after yasnippet)

; (use-package org
;   :defer t
;   :mode ("\\.org\\'" . org-mode)
;   :ensure org-plus-contrib
;   :init
;   (defun my-org-mode-hooks ()
;     (visual-line-mode)
;     (display-line-numbers-mode t)
;     (flyspell-mode)
;     (outline-minor-mode)
;     (electric-pair-mode))
;   (add-hook 'org-mode-hook 'my-org-mode-hooks)
;   :general
;   (despot-def org-mode-map
;     "me"   'org-export-dispatch
;     "mt"   'org-hide-block-toggle
;     "mx"   'org-babel-execute-src-block
;     "mX"   'org-babel-execute-and-next
;     "md"   'org-babel-remove-result)
;   :config
;   (if (not (featurep 'ox-bibtex))
;       (require 'ox-bibtex))
;   (defun org-babel-execute-and-next ()
;     (interactive)
;     (progn (org-babel-execute-src-block)
;            (org-babel-next-src-block)))
;   (setq org-highlight-latex-and-related '(entities script latex)
;         org-tags-column 90)
;   (add-to-list 'org-structure-template-alist
;                '("<ip" "#+BEGIN_SRC ipython :session ? :results raw
;   drawer\n\n#+END_SRC"
;                  "<src lang=\"?\">\n\n</src>")))

; (use-package org-bullets
;   :hook (org-mode . org-bullets-mode))

; (use-package org-pomodoro
;   :general
;   (despot-def org-mode-map
;    "mps"  'org-pomodoro))

(use-package js2-mode
  :ensure t
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t))

(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (defun rjsx-mode-config ()
    "Configure RJSX Mode"
    (define-key rjsx-mode-map (kbd "C-j") 'rjsx-delete-creates-full-tag))
  (add-hook 'rjsx-mode-hook 'rjsx-mode-config))

(use-package slim-mode :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

; (use-package color-theme-sanityinc-tomorrow
;     :ensure t
; 		:config
; 		(load-theme 'sanityinc-tomorrow-night t))


;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))

; (use-package esh-autosuggest
;   :hook (eshell-mode-hook . esh-autosuggest-mode)
;   :ensure t)

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

;;; Emacs Lisp mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; (yas-minor-mode t)
            (eldoc-mode)
            (highlight-symbol-mode)
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

; (defun my--config-evil ()
;   "Configure evil mode."

;   ;; Use Emacs state in these additional modes.
;   (dolist (mode '(ag-mode
;                   custom-mode
;                   custom-new-theme-mode
;                   dired-mode
;                   eshell-mode
;                   flycheck-error-list-mode
;                   git-rebase-mode
;                   org-capture-mode
;                   sunshine-mode
;                   term-mode))
;     (add-to-list 'evil-emacs-state-modes mode))

;   (delete 'term-mode evil-insert-state-modes)
;   (delete 'eshell-mode evil-insert-state-modes)

;   ;; Use insert state in these additional modes.
;   ; (dolist (mode '(twittering-edit-mode
;   ;                 magit-log-edit-mode))
;   ;   (add-to-list 'evil-insert-state-modes mode))

;   (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

;   (evil-add-hjkl-bindings occur-mode-map 'emacs
;     (kbd "/")       'evil-search-forward
;     (kbd "n")       'evil-search-next
;     (kbd "N")       'evil-search-previous
;     (kbd "C-d")     'evil-scroll-down
;     (kbd "C-u")     'evil-scroll-up
;     (kbd "C-w C-w") 'other-window)

;   ;; Global bindings.
;   (evil-define-key 'normal global-map (kbd "<down>")  'evil-next-visual-line)
;   (evil-define-key 'normal global-map (kbd "<up>")    'evil-previous-visual-line)
;   ; (evil-define-key 'normal global-map (kbd "C-p")     'helm-projectile)
;   ; (evil-define-key 'normal global-map (kbd "C-S-p")   'helm-projectile-switch-project)
;   (evil-define-key 'normal global-map (kbd "SPC SPC") 'save-buffer)
;   (evil-define-key 'normal 'global "ge" "`.")

;   (defun minibuffer-keyboard-quit ()
;     "Abort recursive edit.
; In Delete Selection mode, if the mark is active, just deactivate it;
; then it takes a second \\[keyboard-quit] to abort the minibuffer."
;     (interactive)
;     (if (and delete-selection-mode transient-mark-mode mark-active)
;         (setq deactivate-mark  t)
;       (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
;       (abort-recursive-edit)))

;   ;; Make escape quit everything, whenever possible.
;   (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
;   (define-key evil-visual-state-map [escape] 'keyboard-quit)
;   (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;   (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;   (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;   (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;   (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

; (use-package evil
;   :ensure t
;   :init
;   (setq evil-want-C-u-scroll t
;    ;; don't move back the cursor one position when exiting insert mode
;    evil-move-cursor-back nil)
;   (fset 'evil-visual-update-x-selection 'ignore)
;   :commands (evil-mode evil-define-key)
;   :config
;   (add-hook 'evil-mode-hook 'my--config-evil)
;   (evil-mode 1)
;   (use-package evil-surround
;     :ensure t
;     :config
;     (global-evil-surround-mode))

;   (use-package evil-indent-textobject
;     :ensure t)

;   (use-package evil-escape
;     :ensure t
;     :config
;     (global-set-key (kbd "C-SPC") 'evil-escape)
;     (add-hook 'evil-escape-hook 'evil-ex-nohighlight))

;   (use-package evil-commentary
;     :diminish evil-commentary-mode
;     :config (evil-commentary-mode)))

(server-start)

;; (use-package powerline-evil
;;   :ensure t)

(electric-pair-mode 1)
(electric-indent-mode 1)
(setq-default electric-indent-inhibit t)

; (fset 'evil-visual-update-x-selection 'ignore)

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
