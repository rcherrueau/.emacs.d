;; IRC Client for Emacs.
;; Integrates well with the rest of the editor, using standard Emacs
;; key bindings and indicating activity in channels in the status bar
;; so it stays out of your way unless you want to use it.
;; https://github.com/jorgenschaefer/circe/wiki

;; Fly-spell, default is fr
(setq lui-flyspell-p t
      lui-flyspell-alist '((".*" "fr")))

;; Strip mIRC Color Codes
(eval-after-load 'circe
  '(defun lui-irc-propertize (&rest args)))

;; Stop tracking event on bitlbee general buffer
(setq tracking-ignored-buffers '("localhost:6667"))

;; Identifies yourself on bitlbee.
;; Identificaion is done only if bitlbee-nick and bitlbee-password are
;; defined in private feature
(when (featurep 'private)
  (setq circe-network-options
        `(("Bitlbee"
           :nick ,bitlbee-nick
           :nickserv-password ,bitlbee-password))))

(enable-circe-color-nicks)
(enable-circe-highlight-all-nicks)

;; Launch bitlbee
(defun bitlbee ()
  "Connect to bitlbee"
  (interactive)
  ;; with save-window-excursion, the localhost:6667 buffer is not
  ;; opened. This only open &bitlbee channel
  (save-window-excursion
    (circe "Bitlbee")))

(provide 'setup-circe)
