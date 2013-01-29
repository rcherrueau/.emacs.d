(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (org-back-to-heading)
      (org-update-parent-todo-statistics))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; Flyspell
(add-hook 'org-mode-hook 'flyspell-mode)

;; Auto-fill 70
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq-default fill-column 70)

;; Todo Keywords
(setq org-todo-keywords
  '(
    (sequence "TODO" "FEEDBACK" "|" "DONE" "DELEGATED" "INVALID" "FIXED")
    ))

;; Todo Special Colors
;; http://orgmode.org/manual/Faces-for-TODO-keywords.html
(setq org-todo-keyword-faces
  '(("FEEDBACK" . "orange")))

(provide 'setup-org)
