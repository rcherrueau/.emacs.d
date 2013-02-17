;; https://github.com/rooney/zencoding
;; Write HTML and CSS quicker with with Zen Coding
(require 'zencoding-mode)

;; Auto-start zencoding as minor mode of sgml
(add-hook 'sgml-mode-hook 'zencoding-mode)

(provide 'setup-html-mode)
