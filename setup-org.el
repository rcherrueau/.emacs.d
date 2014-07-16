(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (org-back-to-heading)
      (org-update-parent-todo-statistics))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; Disable M-p/n binding for org-shitfup/down
;; Let's use M-p/n for backward/forward-paragraph instead (as it's
;; definied in key-binding.el)
(define-key org-mode-map "\M-n" nil)
(define-key org-mode-map "\M-p" nil)

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

;; Ditaa support
;; http://ditaa.sourceforge.net/
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)))
;; ditaa jar path
(setq org-ditaa-jar-path
      "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")

;; PlantUML support
;; http://plantuml.sourceforge.net/
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))
;; PlantUML jar path
(setq org-plantuml-jar-path
      (expand-file-name "~/Library/PlantUML/plantuml.jar"))


(provide 'setup-org)
