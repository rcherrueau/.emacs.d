;; Setup malabar-mode

;; malabar-mode :: A better Java mode for Emacs
;; https://github.com/espenhw/malabar-mode

;; Or enable more if you wish
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)
(require 'malabar-mode)
(setq malabar-groovy-lib-dir
      (expand-file-name "lib"
                        (expand-file-name "malabar-mode" site-lisp-dir)))
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

(provide 'setup-malabar)
