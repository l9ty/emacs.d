(when (and  (maybe-require-package 'pyim)
            (maybe-require-package 'pyim-basedict))

  (defun gosu/setup-pyim ()
    (require 'pyim)
    (require 'pyim-basedict)

    (pyim-basedict-enable)
    (setq default-input-method "pyim")
    (setq pyim-page-length 5)
    (pyim-default-scheme 'quanpin)
    (setq pyim-cloudim 'baidu)
    (setq-default pyim-punctuation-translate-p '(no)) ;; 使用半角标点

    (setq-default pyim-english-input-switch-functions
                  '(pyim-probe-dynamic-english
                    pyim-probe-isearch-mode
                    pyim-probe-program-mode
                    pyim-probe-org-structure-template))

    (setq-default pyim-punctuation-half-width-functions
                  '(pyim-probe-punctuation-line-beginning
                    pyim-probe-punctuation-after-punctuation))

    (global-set-key (kbd "M-j") 'pyim-convert-string-at-point))

  (add-hook 'after-init-hook 'gosu/setup-pyim))

(provide 'init-pyim)
