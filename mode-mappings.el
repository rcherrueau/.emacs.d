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

;; ProVerif
(let ((mode-msg "Major mode for editing ProVerif code."))
  (autoload 'proverif-pv-mode "proverif" mode-msg t)
  (autoload 'proverif-pi-mode "proverif" mode-msg t)
  (autoload 'proverif-horn-mode "proverif" mode-msg t)
  (autoload 'proverif-horntype-mode "proverif" mode-msg t))

(add-to-list 'auto-mode-alist '("\\.horn$" . proverif-horn-mode))
(add-to-list 'auto-mode-alist '("\\.horntype$" . proverif-horntype-mode))
(add-to-list 'auto-mode-alist '("\\.pv$" . proverif-pv-mode))
(add-to-list 'auto-mode-alist '("\\.pi$" . proverif-pi-mode))

(provide 'mode-mappings)
