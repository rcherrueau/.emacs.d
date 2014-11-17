;; New scala major mode for emacs 24. It is a complete rewrite based
;; on the Scala Language Specification 2.9.
;; See https://github.com/hvesalai/scala-mode2
(require 'scala-mode2)

;; --------------------------------------------------- Indenting modes
;; Strictly function style for run-on lines:
;; val x = foo("bar")
;;            ("zot", "kala") // indented as curry
;;
;; val y = foo("bar")
;;
;; ("zot", "kala") // a tuple
(setq scala-indent:default-run-on-strategy 0)

;; Parameters lists align under the first paramter
;; val y = List( "Alpha", "Bravo",
;;               "Charlie" )
(setq scala-indent:align-parameters t)

;; Expression forms (if, for and try) are aligned
;; val x = if (kala)
;;           foo
;;         else if (koira)
;;           bar
;;         else
;;           zot
(setq scala-indent:align-forms t)

(add-hook 'scala-mode-hook '(lambda ()
  ;; Bind the backtab (shift tab) to
  ;; 'scala-indent:indent-with-reluctant-strategy command. This is
  ;; usefull when using the 'eager' mode by default and you want to
  ;; "outdent" a code line as a new statement.
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)
))

(provide 'setup-scala)
