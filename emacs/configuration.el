(load-file (expand-file-name "sensible-defaults/sensible-defaults.el" user-emacs-directory))
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(server-start)

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

;; (defun oookli/rename-file (new-name)
;;   (interactive "FNew name: ")
;;   (let ((filename (buffer-file-name)))
;;     (if filename
;;         (progn
;;           (when (buffer-modified-p)
;;              (save-buffer))
;;           (rename-file filename new-name t)
;;           (kill-buffer (current-buffer))
;;           (find-file new-name)
;;           (message "Renamed '%s' -> '%s'" filename new-name))
;;       (message "Buffer '%s' isn't backed by a file!" (buffer-name)))))

;; (defun oookli/generate-scratch-buffer ()
;; 	"Create and switch to a temporary scratch buffer with a random
;; 		 name."
;; 	(interactive)
;; 	(switch-to-buffer (make-temp-name "scratch-")))

(defun oookli/kill-current-buffer ()
	"Kill the current buffer without prompting."
	(interactive)
	(kill-buffer (current-buffer)))

;; (defun oookli/visit-last-migration ()
;; 	"Open the most recent Rails migration. Relies on projectile."
;; 	(interactive)
;; 	(let ((migrations
;; 				 (directory-files
;; 					(expand-file-name "db/migrate" (projectile-project-root)) t)))
;; 		(find-file (car (last migrations)))))

(defun oookli/add-auto-mode (mode &rest patterns)
	"Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
	(dolist (pattern patterns)
		(add-to-list 'auto-mode-alist (cons pattern mode))))

;; (defun oookli/find-file-as-sudo ()
;; 	(interactive)
;; 	(let ((file-name (buffer-file-name)))
;; 		(when file-name
;; 			(find-alternate-file (concat "/sudo::" file-name)))))

;; (defun oookli/region-or-word ()
;; 	(if mark-active
;; 			(buffer-substring-no-properties (region-beginning)
;; 																			(region-end))
;; 		(thing-at-point 'word)))

(defun oookli/append-to-path (path)
	"Add a path both to the $PATH variable and to Emacs' exec-path."
	(setenv "PATH" (concat (getenv "PATH") ":" path))
	(add-to-list 'exec-path path))

(use-package which-key
  :config (which-key-mode 1))

(use-package general
  :after which-key
  :config
	      (setq mac-command-modifier 'super)
  (general-override-mode 1)
	      (defun oookli/split-window-below-and-switch ()
			      "Split the window horizontally, then switch to the new pane."
			      (interactive)
			      (split-window-below)
			      (balance-windows)
			      (other-window 1))

	      (defun oookli/split-window-right-and-switch ()
			      "Split the window vertically, then switch to the new pane."
			      (interactive)
			      (split-window-right)
			      (balance-windows)
			      (other-window 1))

  (general-create-definer tyrant-def
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :non-normal-prefix "C-s")

  (tyrant-def
    ""     nil
		      "-"    'oookli/split-window-below-and-switch
		      "/"    'oookli/split-window-right-and-switch
    "SPC"  'save-buffer))

(use-package evil
	:hook (after-init . evil-mode)
	:init
	(setq
		evil-want-C-u-scroll t
		;; don't move back the cursor one position when exiting insert mode
		evil-move-cursor-back nil)
	:config
	;; (evil-set-initial-state 'shell-mode 'normal)
	;; (evil-set-initial-state 'doc-view-mode 'normal)
	;; (evil-set-initial-state 'package-menu-mode 'normal)
	;; (evil-set-initial-state 'biblio-selection-mode 'motion)
	(setq doc-view-continuous t)
	:general
	(tyrant-def
		;; "wh"  'evil-window-left
		;; "wl"  'evil-window-right
		;; "wj"  'evil-window-down
		;; "wk"  'evil-window-up
		"x"  'oookli/kill-current-buffer
		"n"  'evil-buffer-new
		"fd"  'evil-save-and-close))
	;; ('motion override-global-map
	;;  "]b"  'evil-next-buffer
	;;  "[b"  'evil-prev-buffer)

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1)
  :general
  ('normal override-global-map
			      "gc"  'evil-commentary
			      "gC" 'evil-commentary-line))

(use-package evil-escape
  :ensure t
  :config
  (add-hook 'evil-escape-hook 'evil-ex-nohighlight)
  :general
  (general-def "C-SPC" 'evil-escape))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

(set-window-scroll-bars (minibuffer-window) nil nil)

(setq frame-title-format '((:eval (projectile-project-name))))

(global-prettify-symbols-mode t)

(use-package solarized-theme
    :init
    (setq solarized-scale-org-headlines nil)
    (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))
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

(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(setq ring-bell-function 'ignore)

(global-hl-line-mode)

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package ag
  :commands (ag ag-project)
  :config
  (setq ag-executable "/usr/local/bin/ag")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

(use-package company
  :hook (add-hook 'after-init-hook 'global-company-mode)
  :general
  (general-def "M-/" 'company-complete-common))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  :general
  (tyrant-def
    "."  'dumb-jump-go))

(use-package flycheck)

;; (use-package projectile
;; :ensure t
;; :defer 1
;; :config
;; (projectile-mode)
;; (setq projectile-project-search-path '("~/Sites/"))
;; (setq projectile-enable-caching t)
;; (setq projectile-mode-line
;; 	'(:eval
;; 	(format " Proj[%s]"
;; 		(projectile-project-name)))))

(use-package projectile
	:config
	(setq projectile-completion-system 'ivy)
	(setq projectile-switch-project-action 'projectile-dired)
	(setq projectile-require-project-root nil)
  (projectile-global-mode)
	:general
	('normal '(override-global-map ag-mode-map rspec-mode-map)
			"C-p" 'projectile-find-file)
	(tyrant-def
		","  'projectile-ag))

(use-package undo-tree)

(setq-default tab-width 2)

(use-package subword
	:config (global-subword-mode 1))
;; (superword-mode 1)

(setq compilation-scroll-output t)

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package scss-mode
  :config
  (setq scss-compile-at-save nil))

(use-package less-css-mode)

(use-package haml-mode
      :config
(add-hook 'haml-mode-hook 'rspec-mode))

(use-package slim-mode
      :config
(add-hook 'slim-mode-hook 'rspec-mode))

(use-package coffee-mode)

(setq js-indent-level 2)

(add-hook 'coffee-mode-hook
          (lambda ()
            (yas-minor-mode 1)
            (setq coffee-tab-width 2)))

(use-package paredit)

(use-package rainbow-delimiters)

(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package flycheck-package)

(eval-after-load 'flycheck
  '(flycheck-package-setup))

(setq oookli/ruby-version "2.5.3")

(use-package chruby
  :config
  (chruby oookli/ruby-version))

(oookli/append-to-path (format "~/.gem/ruby/%s/bin" oookli/ruby-version))

(use-package rspec-mode)

(setq-default flycheck-disabled-checkers '(ruby-reek))

(setq ruby-align-to-stmt-keywords '(def if))

(use-package yard-mode)

(use-package ruby-end)

(use-package projectile-rails
  :config
  (projectile-rails-global-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ruby-insert-encoding-magic-comment nil)
            (yas-minor-mode)
            (rspec-mode)
            (yard-mode)
            (flycheck-mode)
            (local-set-key "\r" 'newline-and-indent)
            (setq rspec-command-options "--color --order random")
            (chruby-use-corresponding)))

(oookli/add-auto-mode
 'ruby-mode
 "\\Gemfile$"
 "\\.rake$"
 "\\.gemspec$"
 "\\Guardfile$"
 "\\Rakefile$"
 "\\Vagrantfile$"
 "\\Vagrantfile.local$")

(add-hook 'rspec-compilation-mode-hook
          (lambda ()
            (make-local-variable 'compilation-scroll-output)
            (setq compilation-scroll-output 'first-error)))

(add-hook 'web-mode-hook
          (lambda ()
            (rainbow-mode)
            (rspec-mode)
            (setq web-mode-markup-indent-offset 2)))

(oookli/add-auto-mode
 'web-mode
 "\\.erb$"
 "\\.html$"
 "\\.php$"
 "\\.rhtml$")

(use-package yaml-mode
      :config
	      (add-hook 'yaml-mode-hook 'rspec-mode))

(use-package counsel
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-s" . 'swiper)

  :config
  (use-package flx)
  (use-package smex)

  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
	      :general
	      (general-def :keymaps '(ivy-occur-mode-map ivy-occur-grep-mode-map ivy-minibuffer-map)
	      "C-j" 'ivy-next-line
	      "C-k" 'ivy-previous-line)
	      (tyrant-def "\\" 'ivy-switch-buffer))

(use-package wgrep
:config
(wgrep-change-to-wgrep-mode))

;; (eval-after-load 'grep
;;   '(define-key grep-mode-map
;;     (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

;; (eval-after-load 'wgrep
;;   '(define-key grep-mode-map
;;     (kbd "C-c C-c") 'wgrep-finish-edit))

;; (setq wgrep-auto-save-buffer t)

(use-package multi-term
:ensure t
:config
	(setq multi-term-program "/bin/zsh")
	;; (setq multi-term-program-switches "--login")
	(evil-set-initial-state 'term-mode 'emacs)
:general
	(tyrant-def
			"t" 'multi-term))

(defun oookli/term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(add-hook 'term-mode-hook
          (lambda ()
            (goto-address-mode)
            (define-key term-raw-map (kbd "C-y") 'oookli/term-paste)
            (define-key term-raw-map (kbd "<mouse-2>") 'oookli/term-paste)
            (define-key term-raw-map (kbd "M-o") 'other-window)
            (setq yas-dont-activate t)))

;; (global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-o") 'other-window)
