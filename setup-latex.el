;; Flyspell
(add-hook 'latex-mode-hook 'flyspell-mode)

;; Auto-fill 70
(add-hook 'latex-mode-hook 'auto-fill-mode)
(setq-default fill-column 70)

(provide 'setup-latex)
