;; http://www.nongnu.org/geiser
;; Geiser is a collection of Emacs major and minor modes that conspire
;; with one or more Scheme interpreters to keep the Lisp Machine
;; Spirit alive. Use for racket!
(require 'geiser)

;; Set racket as the default REPL
(setq geiser-active-implementations '(racket))

(provide 'setup-geiser)
