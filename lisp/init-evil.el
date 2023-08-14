;; evil

(require-package 'evil)
(add-hook 'after-init-hook 'evil-mode)
(setq evil-disable-insert-state-bindings t)
(when (maybe-require-package 'undo-fu)
  (setq evil-undo-system 'undo-fu))

(with-eval-after-load 'evil

  ;; Bug: key-bindings is invalid when first startup.
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'ibuffer-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'rg-mode 'emacs)
  (evil-set-initial-state 'diff-mode 'emacs)
  ;; (evil-set-initial-state 'compilation-mode 'emacs)

  (setq gosu/evil-key-state '(normal visual motion))
  (evil-set-leader gosu/evil-key-state (kbd "<SPC>"))

  (define-key global-map (kbd "<leader><SPC>") 'execute-extended-command)

  (defun gosu/define-evil-prefix (sym key)
    (define-prefix-command sym)
    (evil-define-key gosu/evil-key-state global-map (kbd (concat "<leader>" key)) sym))

  (global-set-key (kbd "<leader> .") 'xref-find-definitions)
  (global-set-key (kbd "<leader> ,") 'xref-pop-marker-stack)
  (global-set-key (kbd "<leader> ?") 'xref-find-references)

  (global-set-key (kbd "<leader> `") 'save-buffers-kill-terminal)
  (global-set-key (kbd "<leader> b") 'consult-buffer)
  ;;  (global-set-key (kbd "<leader> B") 'ibuffer)
  (global-set-key (kbd "<leader> i") 'consult-imenu)
  (global-set-key (kbd "<leader> o") 'switch-window)
  (global-set-key (kbd "<leader> k") 'kill-current-buffer)
  (global-set-key (kbd "<leader> K") 'kill-other-buffers)
  (global-set-key (kbd "<leader> f") 'find-file)
  (global-set-key (kbd "<leader> l") 'consult-line)
  (global-set-key (kbd "<leader> c") 'avy-goto-char-timer)
  (global-set-key (kbd "<leader> 1") 'sanityinc/toggle-delete-other-windows)
  (global-set-key (kbd "<leader> 0") 'delete-window)
  (global-set-key (kbd "<leader> 2")
                  (split-window-func-with-other-buffer 'split-window-vertically))
  (global-set-key (kbd "<leader> 3")
                  (split-window-func-with-other-buffer 'split-window-horizontally))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "<leader> p") 'projectile-command-map))

  ;; (gosu/define-evil-prefix '+register "r")
  ;; (define-key +register (kbd "r") 'point-to-register)
  ;; (define-key +register (kbd "p") 'jump-to-register)
  ;; (define-key +register (kbd "l") 'consult-register)

  (gosu/define-evil-prefix '+narrow "n")
  (define-key +narrow (kbd "n") 'narrow-to-region)
  (define-key +narrow (kbd "w") 'widen)
  (define-key +narrow (kbd "d") 'narrow-to-defun)
  ;; (define-key +narrow (kbd "p") 'narrow-to-page)

  (defun insert-dirtree ()
    (interactive)
    (insert "├─ "))
  (defun insert-dirtree-end ()
    (interactive)
    (insert "└─ "))

  (global-set-key (kbd "<leader> d") 'insert-dirtree)
  (global-set-key (kbd "<leader> D") 'insert-dirtree-end)

  (with-eval-after-load 'elisp-slime-nav
    (define-key elisp-slime-nav-mode-map
      (kbd "<leader> .") 'elisp-slime-nav-find-elisp-thing-at-point)))

(provide 'init-evil)
