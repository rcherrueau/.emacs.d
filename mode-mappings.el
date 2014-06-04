;; Emacs lisp
(add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; Adventur
(autoload 'adventur-mode "adventur-mode")
(add-to-list 'auto-mode-alist '("\\.adv$" . adventur-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))
(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

;; Clojure
(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; Configuration files
(add-to-list 'auto-mode-alist '("\\.offlineimaprc$" . conf-mode))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

(provide 'mode-mappings)
