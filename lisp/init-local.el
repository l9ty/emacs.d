(require 'init-evil)
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
  "Kill unvisible buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((name (buffer-name buf)))
      (when (not (or (string-prefix-p " " name)
                     (string-match-p "^\\*scratch\\$*" name)
                     (get-buffer-window buf 'visible)))
        (kill-buffer buf)))))

(defun gosu/surround-with (str)
  (interactive "sEnter the string: ")
  (when (string-empty-p str)
    (setq str "="))
  (let ((start (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char start)
      (insert str)
      (goto-char (+ end 1))
      (insert str))))


;; cc

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(setq-default c-basic-offset 4)

(add-hook 'c-mode-common-hook 'hs-minor-mode)
;; (add-hook 'c-mode-common-hook 'superword-mode)

(setq-default gdb-many-windows t)
(setq-default gdb-default-window-configuration-file "gdb-windows.el")

(when (maybe-require-package 'hydra)
  (with-eval-after-load 'gud
    (defhydra hydra-gud (:color pink :hint nil)
      "
_B_: break          _s_: step     _g_: until
_t_: tbreak         _n_: next     _G_: jump
_d_: delete break   _p_: print    _f_: finish

_r_: run            _N_: continue
"
      ("B" gud-break)
      ("t" gud-tbreak)
      ("d" gud-remove)
      ("s" gud-step)
      ("n" gud-next)
      ("p" gud-print)
      ("g" gud-until)
      ("G" gud-jump)
      ("f" gud-finish)
      ("r" gud-run)
      ("N" gud-go)
      ("q" nil "cancel"))

    (define-key c-mode-map (kbd "<leader> d") 'hydra-gud/body)))

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

(when (maybe-require-package 'valign)
  (add-hook 'org-mode-hook 'valign-mode))



(when (maybe-require-package 'nasm-mode)
  (add-to-list 'auto-mode-alist '("\\.[sS]\\'" . nasm-mode))
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode)))

(when (maybe-require-package 'expand-region)
  (global-set-key (kbd "C-\\") 'er/expand-region))

(when (maybe-require-package 'yasnippet)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (with-eval-after-load 'yasnippet
    (yas-reload-all))
  )

;; TODO: remove this if debian fdfind upgrade to v8.3.0+
;; https://github.com/bbatsov/projectile/issues/1788
(setq projectile-generic-command "fdfind . -0 --type f --color=never")
(setq projectile-git-fd-args "-H -0 -E .git -tf")

(provide 'init-local)
