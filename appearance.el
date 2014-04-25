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

;; Font size and appearance
(when window-system
  (if is-mac
      (progn
        (set-frame-font "Ubuntu Mono-13" nil t)
        ; Ubuntu Mono for greek letter.
        (set-fontset-font "fontset-default"
                          '(#x374 . #x3FF)
                          (font-spec :family "Ubuntu Mono")))
    (set-frame-font "Ubuntu Mono-11" nil t))
  ; DejaVu Sans Mono for mathematical symbol
  (set-fontset-font "fontset-default"
                    '(#x2190 . #x22ff)
                    (font-spec :family "DejaVu Sans Mono"))
  (setq face-font-rescale-alist '((".*DejaVu.Sans.Mono.*" . 0.9))))


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
