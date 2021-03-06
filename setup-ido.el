;; Interactively Do Things
;; http://emacswiki.org/emacs/InteractivelyDoThings
;; Interactive list in mode line durring file and buffer search
(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

(add-hook
 'ido-setup-hook
 #'(lambda ()

     ;; Use C-w to go back up a dir to better match normal usage of C-w
     ;; - insert current file name with C-x C-w instead.
     (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
     (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

;; Use ido everywhere
;; https://github.com/DarwinAwardWinner/ido-ubiquitous
(require 'ido-ubiquitous)
(setq ido-everywhere t)
(ido-ubiquitous-mode 1)

;; ;; Fix ido-ubiquitous for newer packages
;; (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
;;   `(eval-after-load ,package
;;      '(defadvice ,cmd (around ido-ubiquitous-new activate)
;;         (let ((ido-ubiquitous-enable-compatibility nil))
;;           ad-do-it))))

;; (ido-ubiquitous-use-new-completing-read webjump 'webjump)
;; Smart M-x is smart

;; https://github.com/nonsequitur/smex
;; Smex is a M-x enhancement for Emacs. Built on top of Ido, it
;; provides a convenient interface to your recently and most
;; frequently used commands. And to all the other commands, too.
(require 'smex)
(smex-initialize)

(provide 'setup-ido)
