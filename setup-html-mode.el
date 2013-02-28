;; Set sgml/html tab width
(setq sgml-basic-offset 2)

;; Set javascript tab width
(setq js-indent-level 2)

;; https://github.com/rooney/zencoding
;; Write HTML and CSS quicker with with Zen Coding
(require 'zencoding-mode)

;; Auto-start zencoding as minor mode of sgml/html
(add-hook 'sgml-mode-hook 'zencoding-mode)

(provide 'setup-html-mode)

