;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))
(add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Are we on a mac?
;; Note: Emacs runs .bashrc in *shell*
;; So mac users should ln -s .profile .bashrc
(setq is-mac (equal system-type 'darwin))

;; Setup packages
;; Repistories and definition in setup-package.el
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   ;; https://github.com/purcell/exec-path-from-shell
   ;; A GNU Emacs library to setup environment variables from the user's shell
   ;; Avoid OS X problem with environment varaiables
   (cons 'exec-path-from-shell melpa)
   ;; http://philjackson.github.com/magit/
   ;; An emacs mode for git.
   (cons 'magit melpa)
   ;; https://github.com/defunkt/gist.el
   ;; Yet another Emacs paste mode, this one for Gist. Contribute to
   ;; gist.el development by creating an account on GitHub.
   ;; Gist is a simple way to share snippets and pastes with others
   (cons 'gist melpa)
   ;; http://emacswiki.org/emacs/Htmlize
   ;; Generate HTML output of emacs buffer
   (cons 'htmlize melpa)
   ;;
   (cons 'elisp-slime-nav melpa)
   ;;
   (cons 'slime-js marmalade)
   ;; https://github.com/lunaryorn/git-modes
   ;; Mode for git commit messages, helps format the commit messages
   ;; according to convention:
   ;; http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
   (cons 'git-commit-mode melpa)
   ;; https://github.com/lunaryorn/git-modes
   ;; Mode for editing gitconfig file
   (cons 'gitconfig-mode melpa)
   ;; https://github.com/lunaryorn/git-modes
   ;; Mode for editing gitignore file
   (cons 'gitignore-mode melpa)
   ;; https://github.com/nex3/perspective-el
   ;; Provides multiple workspace that makes it easy to work on many
   ;; separate projects without getting lost in all the buffers.
   (cons 'perspective marmalade)
   ;; https://github.com/technomancy/find-file-in-project
   ;; Quickly find any file in a given project.
   (cons 'find-file-in-project marmalade)
   ;; https://github.com/bbatsov/solarized-emacs
   ;; Solarized theme for emacs
   (cons 'solarized-theme melpa)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac (exec-path-from-shell-initialize))

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-hippie)
(require 'setup-perspective)
(require 'setup-ffip)
(require 'setup-html-mode)

;; Language specific setup files
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Load slime-js when asked for
(autoload 'slime-js-jack-in-browser "setup-slime-js" nil t)
(autoload 'slime-js-jack-in-node "setup-slime-js" nil t)

;; Map files to modes
(require 'mode-mappings)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'expand-region)
(require 'mark-more-like-this)
(require 'inline-string-rectangle)
(require 'multiple-cursors)
(require 'delsel)
(require 'jump-char)
(require 'eproject)
(require 'wgrep)
(require 'smart-forward)
(require 'change-inner)
(require 'multifiles)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

;; Misc
(require 'appearance)
(require 'my-misc)
(when is-mac (require 'mac))

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
; (eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

;; Email, baby
(require 'setup-mu4e)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
