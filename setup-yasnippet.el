;; https://github.com/capitaomorte/yasnippet
;; YASnippet is a template system for Emacs. It allows you to type an
;; abbreviation and automatically expand it into function templates.
(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" ;; Personal snippets
        ))
(yas-global-mode 1)

(provide 'setup-yasnippet)
