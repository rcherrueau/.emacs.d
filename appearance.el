(require 'fill-column-indicator)

;; Hide tools, scroll & menu-bar
;; Change frame title, add abolute path to buffer name
(when window-system
  (tool-bar-mode -1)
  ; (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; Theme
(load-theme 'zenburn t)

;; Font size to 11pt
(if window-system
    (set-face-attribute 'default nil
                        :font (if is-mac
                                  "Ubuntu Mono-13"
                                "Ubuntu Mono-11")))

;; Highlight current line
(global-hl-line-mode 1)

;; Indicate the location of the fill column
(setq fci-rule-width 5)

;; get faces with M-x liste-faces-display ;)
;(setq fci-rule-color (face-attribute 'highlight :background))

;; Flash the frame to represent a bell
(setq visible-bell t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; end of apparance.el
(provide 'appearance)
