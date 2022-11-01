(defun gosu/reset-font ()

  (defun font-exist-p (font)
    (member font (font-family-list))
    ;; (x-list-fonts font)
    )

  (when (display-graphic-p)
    (let ((ascii-height 130)
          (chinese-fsize 20))

      (dolist (ascii-font '("Source Code Pro" "Fira Code"))
        (when (font-exist-p ascii-font)
          (set-face-attribute
           'default nil :family ascii-font :height ascii-height)))

      (dolist (chinese-font '("WenQuanYi Micro Hei Mono"
                              "Microsoft YaHei UI"))
        (when (font-exist-p chinese-font)
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font
             (frame-parameter nil 'font) charset
             (font-spec :family chinese-font :size chinese-fsize)))))
      )))
(add-hook 'after-init-hook 'gosu/reset-font)

(maybe-require-package 'gruvbox-theme)




(setq revert-without-query '(".*"))
(global-set-key [remap kill-buffer] 'kill-current-buffer)

(defun kill-other-buffers ()
  "Kill other buffers excluding scratch."
  (interactive)
  (defun filter (b)
    (let ((name (buffer-name b)))
      (not (or (string-prefix-p " "  name)
               ;; (string-match-p "^\\*Messages\\*$" name)
               (string-match-p "^\\*scratch\\$*" name)
               (string-equal name (buffer-name (current-buffer)))))))
  (mapcar (lambda (b) (kill-buffer b))
          (seq-filter 'filter (buffer-list))))



;; evil
(require-package 'evil)
(add-hook 'after-init-hook 'evil-mode)
(setq evil-disable-insert-state-bindings t)
(when (maybe-require-package 'undo-fu)
  (setq evil-undo-system 'undo-fu))

(with-eval-after-load 'evil

  (setq gosu/evil-key-state '(normal visual))
  (evil-set-leader gosu/evil-key-state (kbd "<SPC>"))

  (define-key global-map (kbd "<leader><SPC>") 'execute-extended-command)

  (defun gosu/define-evil-prefix (sym key)
    (define-prefix-command sym)
    (evil-define-key gosu/evil-key-state global-map (kbd (concat "<leader>" key)) sym))

  (define-key global-map (kbd "<leader>b") 'consult-buffer)
  (define-key global-map (kbd "<leader>i") 'consult-imenu)
  (define-key global-map (kbd "<leader>o") 'switch-window)
  (define-key global-map (kbd "<leader>k") 'kill-current-buffer)
  (define-key global-map (kbd "<leader>f") 'find-file)
  (define-key global-map (kbd "<leader>l") 'consult-line)
  (define-key global-map (kbd "<leader>c") 'avy-goto-char-timer)
  (define-key global-map (kbd "<leader>1") 'sanityinc/toggle-delete-other-windows)
  (define-key global-map (kbd "<leader>;") 'pop-to-mark-command)
  (define-key global-map (kbd "<leader>`") 'save-buffers-kill-terminal)

  (gosu/define-evil-prefix '+register "r")
  (define-key +register (kbd "r") 'point-to-register)
  (define-key +register (kbd "p") 'jump-to-register)
  (define-key +register (kbd "l") 'consult-register)


  ;; (define-prefix-command '+goto)
  ;; (evil-define-key gosu/evil-key-state global-map (kbd "<leader>l") 'consult-line)
  ;; (evil-define-key gosu/evil-key-state global-map (kbd "<leader>L") 'consult-line-multi)

  )




;; cc

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))
(setq-default c-basic-offset 4)

(add-hook 'c-mode-common-hook 'hs-minor-mode)

(with-eval-after-load 'gud
  (setq gdb-many-windows t))

(when (maybe-require-package 'ggtags)
  (with-eval-after-load 'ggtags
    (define-key ggtags-navigation-map (kbd "M-,")
      'ggtags-navigation-mode-abort)))



(provide 'init-local)
