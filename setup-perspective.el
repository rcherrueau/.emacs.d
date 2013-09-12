;; Perspective
;; Provides multiple workspace that makes it easy to work on many
;; separate projects without getting lost in all the buffers.
;; https://github.com/nex3/perspective-el
(require 'perspective)

;; Enable perspective mode
(persp-mode t)

;; Macro to replay perspective creation
;; http://emacsrookie.com/2011/09/25/workspaces/
(defmacro custom-persp (name &rest body)
       `(let ((initialize (not (gethash ,name perspectives-hash)))
              (current-perspective persp-curr))
          (persp-switch ,name)
          (when initialize ,@body)
          (setq persp-last current-perspective)))

;; Perspective big-screen
(defun custom-persp/big-screen ()
  (interactive)
  (custom-persp "@big-screen"
                (big-screen-layout)))

;; Perspective normal-screen
(defun custom-persp/normal-screen ()
  (interactive)
  (custom-persp "@normal-screen"
                (normal-screen-layout)))

;; Jump to last perspective
(defun custom-persp-last ()
  (interactive)
  (persp-switch (persp-name persp-last)))

(define-key persp-mode-map (kbd "C-x p -") 'custom-persp-last)

(provide 'setup-perspective)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perspective layouts

;; Big screen layout
;;
;; +---------------------+-----------------------+---------------------+
;; |                     |                       |  shell              |
;; |                     |                       |                     |
;; |                     |                       |                     |
;; |       scratch       |       timeline        |                     |
;; |                     |                       +---------------------+
;; |                     |                       |  irc                |
;; |                     |                       |                     |
;; |                     |                       |                     |
;; +---------------------+-----------------------+---------------------+
;;
;; Timeline screen required `timeline-filepath' to be set with the
;; filepath of the timeline.
(defun big-screen-layout ()
  ;; Split frame in three horizontal windows
  (let* ((scratch-window (selected-window))
         (timeline-window (split-window-right-of scratch-window))
         (shell-window (split-window-right-of timeline-window))
         (irc-window (split-window-below-of shell-window)))

    (balance-windows)

    ;; Open timeline
    ;; Timeline required `timeline-filepath' set with the filepath of
    ;; timeline.
    (open-in-window timeline-window
                    (lambda () (when timeline-filepath
                                 (find-file timeline-filepath))))

    ;; Open shell
    (open-in-window shell-window (lambda () (shell)))))

;; Normal screen layout
;;
;;  +--------------------+--------------------+
;;  |                    |                    |
;;  |                    |                    |
;;  |                    |  shell             |
;;  |                    |                    |
;;  |                    |                    |
;;  |    timeline        +--------------------+
;;  |                    |                    |
;;  |                    |  scratch           |
;;  |                    |                    |
;;  |                    |                    |
;;  +--------------------+--------------------+
;;
(defun normal-screen-layout ()
  ;; Split frame in three horizontal windows
  (let* ((timeline-window (selected-window))
         (shell-window (split-window-right-of timeline-window))
         (scratch-window (split-window-below-of shell-window)))

    (balance-windows)

    ;; Open timeline
    ;; Timeline required `timeline-filepath' set with the filepath of
    ;; timeline.
    (open-in-window timeline-window
                    (lambda () (when timeline-filepath
                                 (find-file timeline-filepath))))

    ;; Open shell
    (open-in-window shell-window (lambda () (shell)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defun split-window-right-of (window)
  (let ((old-selected-window (selected-window)))
    (select-window window)

    (let ((new-window (split-window-right)))
      (select-window old-selected-window)
      new-window)))

(defun split-window-below-of (window)
  (let ((old-selected-window (selected-window)))
    (select-window window)

    (let ((new-window (split-window-below)))
      (select-window old-selected-window)
      new-window)))

(defun open-in-window (window opening-function)
  (let ((old-selected-window (selected-window)))
    (select-window window)
    (funcall opening-function)
    (select-window old-selected-window)))
