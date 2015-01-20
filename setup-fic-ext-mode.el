;; Show FIXME/TODO/BUG(...) in special face only in comments and
;; strings.
;;
;; To use, save fic-ext-mode.el to a directory in your load-path.
;;
;; (require 'fic-ext-mode)
;; (add-hook 'c++-mode-hook 'fic-ext-mode)
;; (add-hook 'emacs-lisp-mode-hook 'fic-ext-mode)
;;
;; or
;;
;; M-x fic-ext-mode
;;
;; NOTE: If you manually turn on fic-ext-mode, you you might need to
;; force re-fontification initially
;; M-x font-lock-fontify-buffer
(require 'fic-ext-mode)

(add-hook 'c-mode-common-hook 'fic-ext-mode)
(add-hook 'emacs-lisp-mode-hook 'fic-ext-mode)
(add-hook 'scala-mode-hook 'fic-ext-mode)
(add-hook 'scheme-mode-hook 'fic-ext-mode)

(provide 'setup-fic-ext-mode)
