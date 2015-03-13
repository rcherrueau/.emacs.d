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
        ; Ubuntu Mono as general font
        (set-frame-font "Ubuntu Mono-13" nil t)
        ; Ubuntu Mono for greek letter.
        (set-fontset-font "fontset-default"
                          '(#x374 . #x3FF)
                          (font-spec :family "Ubuntu Mono")))
    (set-frame-font "Ubuntu Mono-11" nil t))
  ; Fallback Character Encodong: this character encoding is used for
  ; legacy content that fails to declare its encoding.
  ; DejaVu Sans Mono for mathematical symbol
  (set-fontset-font "fontset-default"
                    ;'(#x2190 . #x22ff)
                    '(#x2020 . #x22ff)
                    (font-spec :family "DejaVu Sans Mono"))
  (setq face-font-rescale-alist '((".*DejaVu.Sans.Mono.*" . 0.9))))


;; Highlight current line
(global-hl-line-mode 1)

;; Flash the frame to represent a bell
(setq visible-bell t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Highlight the following words in comments
(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\|FIXME\\|HACK\\|XXX\\|BUG\\|Note\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'add-watchwords)

;; Transparent Emacs
(set-frame-parameter (selected-frame) 'alpha '(97 97))
(add-to-list 'default-frame-alist '(alpha 97 97))

;; end of apparance.el
(provide 'appearance)
