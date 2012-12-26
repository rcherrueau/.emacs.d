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
(load-theme 'solarized-light t)

;; Font size to 11pt
(set-face-attribute 'default nil :height 110)

;; Highlight current line
(global-hl-line-mode 1)

;; Flash the frame to represent a bell
(setq visible-bell t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Show column number in the mode line
(setq column-number-mode t)

;; end of apparance.el
(provide 'appearance)
