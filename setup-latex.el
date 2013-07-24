;; Flyspell
(add-hook 'latex-mode-hook 'flyspell-mode)

;; Auto-fill 70
(add-hook 'latex-mode-hook 'auto-fill-mode)
(setq-default fill-column 70)


;; Launch reformat and validate after
;; the clean entry on bibentry
(add-hook 'bibtex-clean-entry-hook
          (lambda ()
            (bibtex-reformat)
            (bibtex-validate)))

(provide 'setup-latex)
