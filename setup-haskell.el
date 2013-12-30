;; A major mode for editing Haskell (the functional programming
;; language, see URL `http://www.haskell.org') in Emacs.
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(provide 'setup-haskell)
