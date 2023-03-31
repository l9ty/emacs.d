(require 'init-pyim)

(defun gosu/reset-font ()

  (defun font-exist-p (font)
    (member font (font-family-list))
    ;; (x-list-fonts font)
    )

  (when (display-graphic-p)
    (let ((ascii-height 130)
          (chinese-fsize 16))

      (dolist (ascii-font '("Source Code Pro" "Fira Code"))
        (when (font-exist-p ascii-font)
          (set-face-attribute
           'default nil :family ascii-font :height ascii-height)))

      (dolist (chinese-font '("Noto Sans CJK TC"
                              "WenQuanYi Micro Hei Mono"
                              "Microsoft YaHei UI"))
        (when (font-exist-p chinese-font)
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font
             (frame-parameter nil 'font) charset
             (font-spec :family chinese-font :size chinese-fsize)))))
      )))
(add-hook 'after-init-hook 'gosu/reset-font)

(maybe-require-package 'gruvbox-theme)

(global-set-key (kbd "<f5>") 'revert-buffer)

(when (maybe-require-package 'hl-todo)
  (add-hook 'prog-mode-hook 'hl-todo-mode))


;; Adjust setup from init-*.el

(defun gosu/close-flymake ()
  (flymake-mode -1))

(add-hook 'haskell-mode-hook 'gosu/close-flymake)



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

  ;; Bug: key-bindings is invalid when first startup.

  (setq gosu/evil-key-state '(normal visual motion))
  (evil-set-leader gosu/evil-key-state (kbd "<SPC>"))

  (define-key global-map (kbd "<leader><SPC>") 'execute-extended-command)

  (defun gosu/define-evil-prefix (sym key)
    (define-prefix-command sym)
    (evil-define-key gosu/evil-key-state global-map (kbd (concat "<leader>" key)) sym))

  (global-set-key (kbd "<leader> ,") 'pop-to-mark-command)
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

  ;; (gosu/define-evil-prefix '+register "r")
  ;; (define-key +register (kbd "r") 'point-to-register)
  ;; (define-key +register (kbd "p") 'jump-to-register)
  ;; (define-key +register (kbd "l") 'consult-register)

  (gosu/define-evil-prefix '+narrow "n")
  (define-key +narrow (kbd "n") 'narrow-to-region)
  (define-key +narrow (kbd "w") 'widen)
  (define-key +narrow (kbd "d") 'narrow-to-defun)
  ;; (define-key +narrow (kbd "p") 'narrow-to-page)

  (with-eval-after-load 'elisp-slime-nav
    (define-key elisp-slime-nav-mode-map
      (kbd "<leader> .") 'elisp-slime-nav-find-elisp-thing-at-point)))


;; cc

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(setq-default c-basic-offset 4)


(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook 'superword-mode)

(with-eval-after-load 'gud
  (setq gdb-many-windows t))

(when (maybe-require-package 'ggtags)
  (defun gosu/c-gtags-mode ()
    "Turn on/off the ggtags-mode for c-mode."
    (interactive)

    (require 'ggtags)
    (defun open-gtags ()
      (ggtags-mode 1)
      (flymake-mode -1))
    (if ggtags-mode
        (remove-hook 'c-mode-common-hook 'open-gtags)
      (add-hook 'c-mode-common-hook 'open-gtags))
    (revert-buffer))

  (with-eval-after-load 'ggtags
    (with-eval-after-load 'evil
      (define-key ggtags-mode-map (kbd "<leader>[") 'ggtags-prev-mark)
      (define-key ggtags-mode-map (kbd "<leader>]") 'ggtags-next-mark)
      (define-key ggtags-mode-map (kbd "<leader>.") 'ggtags-find-tag-dwim)
      ;; (define-key ggtags-mode-map (kbd "<leader>,") 'ggtags-navigation-mode-abort)
      (define-key ggtags-mode-map (kbd "<leader>,") 'xref-pop-marker-stack))))


;; lua

(when (package-installed-p 'lua-mode)
  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'lua-mode))
  (setq lua-indent-level 4))

;; Translate

(when (maybe-require-package 'go-translate)
  (require-package 'posframe)

  (setq gts-translate-list '(("en" "zh") ("zh" "en")))

  (with-eval-after-load 'go-translate

    (defun gosu/gts-buffer-render ()
      "Use buffer render to translate."
      (interactive)
      (setq-local gts-default-translator
                  (gts-translator
                   :picker (gts-prompt-picker)
                   :engines (list (gts-google-engine))
                   :render (gts-buffer-render))))

    (setq gts-default-translator
          (gts-translator
           :picker (gts-prompt-picker)
           :engines (list (gts-google-engine)) ; gts-bing-engine
           :render (gts-posframe-pop-render)
           )))

  (with-eval-after-load 'evil
    (global-set-key (kbd "<leader> t") 'gts-do-translate)))


;; Org-Mode

(when (maybe-require-package 'toc-org)
  (add-hook 'org-mode-hook 'toc-org-mode))



(when (maybe-require-package 'expand-region)
  (global-set-key (kbd "C-\\") 'er/expand-region))

;; TODO: yasnippet eglot
(when (maybe-require-package 'yasnippet)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (with-eval-after-load 'yasnippet
    (yas-reload-all))
  )

(provide 'init-local)
