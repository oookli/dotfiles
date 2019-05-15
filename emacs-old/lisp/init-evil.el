;;; init-evil.el -- My evil mode configuration.
;;; Commentary:
;;; Code:
(defun my--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
    "\\"  'helm-mini
    "SPC" 'save-buffer
    "f"   'projectile-switch-project
    "x"   'kill-this-buffer
    ","   'helm-projectile-ag
    "t"   'open-eshell
    "y"   'helm-M-x
    "nr"  'helm-find-files
    "g"   'magit-status
    "v"   'evil-window-vsplit
    "-"   'evil-window-split
    "R"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display)))

    ;; "#"  'server-edit
    ;; ","  'other-window
    ;; "."  'mode-line-other-buffer
    ;; ":"  'eval-expression
    ;; "aa" 'align-regexp
    ;; "a=" 'my-align-single-equals
    ;; "b"  'helm-mini             ;; Switch to another buffer
    ;; "B"  'magit-blame-toggle
    ;; "c"  'comment-dwim
    ;; "d"  'kill-this-buffer
    ;; "D"  'open-current-line-in-codebase-search
    ;; "f"  'helm-imenu            ;; Jump to function in buffer
    ;; "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    ;; "l"  'whitespace-mode       ;; Show invisible characters
    ;; "nn" 'air-narrow-dwim       ;; Narrow to region and enter normal mode
    ;; "nw" 'widen
    ;; "o"  'delete-other-windows  ;; C-w o
    ;; "p"  'helm-show-kill-ring
    ;; "s"  'ag-project            ;; Ag search from project's root
    ;; "r"  'chrome-reload
    ;; "S"  'delete-trailing-whitespace
    ;; "t"  'gtags-reindex
    ;; "T"  'gtags-find-tag
    ;; "w"  'save-buffer
    ;; "x"  'helm-M-x
    ;; "y"  'yank-to-x-clipboard)

  ; (defun magit-blame-toggle ()
  ;   "Toggle magit-blame-mode on and off interactively."
  ;   (interactive)
  ;   (if (and (boundp 'magit-blame-mode) magit-blame-mode)
  ;       (magit-blame-quit)
  ;     (call-interactively 'magit-blame))))
)

(defun my--config-evil ()
  "Configure evil mode."

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
                  custom-mode
                  custom-new-theme-mode
                  dired-mode
                  eshell-mode
                  flycheck-error-list-mode
                  git-rebase-mode
                  org-capture-mode
                  sunshine-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (delete 'term-mode evil-insert-state-modes)
  (delete 'eshell-mode evil-insert-state-modes)

  ;; Use insert state in these additional modes.
  ; (dolist (mode '(twittering-edit-mode
  ;                 magit-log-edit-mode))
  ;   (add-to-list 'evil-insert-state-modes mode))

  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Global bindings.
  (evil-define-key 'normal global-map (kbd "<down>")  'evil-next-visual-line)
  (evil-define-key 'normal global-map (kbd "<up>")    'evil-previous-visual-line)
  (evil-define-key 'normal global-map (kbd "C-p")     'helm-projectile)
  (evil-define-key 'normal global-map (kbd "C-S-p")   'helm-projectile-switch-project)
  (evil-define-key 'normal global-map (kbd "SPC SPC") 'save-buffer)
  (evil-define-key 'normal 'global "ge" "`.")

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package evil
  :ensure t
  :init
  (setq
   evil-want-C-u-scroll t
   ;; don't move back the cursor one position when exiting insert mode
   evil-move-cursor-back nil)
  :commands (evil-mode evil-define-key)
  :config
  (add-hook 'evil-mode-hook 'my--config-evil)
  (evil-mode 1)

  ; (use-package evil-leader
  ;   :ensure t
  ;   :config
  ;   (global-evil-leader-mode)
  ;   (air--config-evil-leader))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t)

  (use-package evil-escape
    :ensure t
    :config
    (global-set-key (kbd "C-SPC") 'evil-escape)
    (add-hook 'evil-escape-hook 'evil-ex-nohighlight))

  (use-package evil-commentary
    :diminish evil-commentary-mode
    :config (evil-commentary-mode)))

(provide 'init-evil)
;;; init-evil.el ends here
