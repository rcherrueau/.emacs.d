;; New scala major mode for emacs 24. It is a complete rewrite based
;; on the Scala Language Specification 2.9.
;; See https://github.com/hvesalai/scala-mode2
(require 'scala-mode2)
;; An emacs mode for interacting with sbt, scala console (aka REPL)
;; and sbt projects.
;; https://github.com/hvesalai/sbt-mode
(require 'sbt-mode)

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

  ;; sbt-find-definitions is a command that tries to find (with grep)
  ;; the definition of the thing at point.
  (local-set-key (kbd "M-.") 'sbt-find-definitions)

  ;; Use sbt-run-previous-command to re-compile your code after
  ;; changes
  (local-set-key (kbd "C-c C-a") 'sbt-run-previous-command)
))

(add-hook 'sbt-mode-hook '(lambda ()
  ;; compilation-skip-threshold tells the compilation minor-mode
  ;; which type of compiler output can be skipped. 1 = skip info
  ;; 2 = skip info and warnings.
  (setq compilation-skip-threshold 1)

  ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
  ;; cursor to just after prompt.
  (local-set-key (kbd "C-a") 'comint-bol)

  ;; Bind M-RET to 'comint-accumulate. This will allow you to add
  ;; more than one line to scala console prompt before sending it
  ;; for interpretation. It will keep your command history cleaner.
  (local-set-key (kbd "M-RET") 'comint-accumulate)
))

;; On 'sbt-run-previous-command skip the question to save buffers and
;; have buffers saved automatically instead.
(setq compilation-ask-about-save nil)



(provide 'setup-scala)
