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
   ;; https://github.com/magnars/dash.el
   ;; A modern list library for Emacs.
   (cons 'dash melpa)
   ;; http://philjackson.github.com/magit/
   ;; An emacs mode for git.
   (cons 'magit melpa)
   ;; https://github.com/defunkt/gist.el
   ;; Yet another Emacs paste mode, this one for Gist. Contribute to
   ;; gist.el development by creating an account on GitHub.
   ;; Gist is a simple way to share snippets and pastes with others
   (cons 'gist melpa)
   ;; http://www.emacswiki.org/emacs/UndoTree
   ;; More advanced yet simpler undo system inspired by Vim
   (cons 'undo-tree melpa)
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
   ;; https://github.com/rooney/zencoding
   ;; Write HTML and CSS quicker with with Zen Coding.
   (cons 'zencoding-mode melpa)
   ;; https://github.com/bbatsov/solarized-emacs
   ;; Solarized theme for emacs
   (cons 'solarized-theme melpa)
   ;; https://github.com/bbatsov/zenburn-emacs
   ;; Zenburn theme for Emacs
   (cons 'zenburn-theme melpa)
   ;; https://github.com/hvesalai/scala-mode2
   ;; New scala major mode for emacs 24. It is a complete rewrite
   ;; based on the Scala Language Specification 2.9.
   (cons 'scala-mode2 melpa)
   ;; http://www.nongnu.org/geiser
   ;; Geiser is a collection of Emacs major and minor modes that
   ;; conspire with one or more Scheme interpreters to keep the Lisp
   ;; Machine Spirit alive. Use for racket!
   (cons 'geiser marmalade)
   ;; https://github.com/alpaker/Fill-Column-Indicator
   ;; Many modern editors and IDEs can graphically indicate the
   ;; location of the fill column by drawing a thin line (in design
   ;; parlance, a `rule') down the length of the editing window.
   ;; Fill-column-indicator implements this facility in Emacs.
   (cons 'fill-column-indicator marmalade)
   ;; https://github.com/haskell/haskell-mode
   ;; A major mode for editing Haskell (the functional programming
   ;; language, see URL `http://www.haskell.org') in Emacs.
   (cons 'haskell-mode marmalade)
   ;; https://github.com/jorgenschaefer/circe Circe is a Client for
   ;; IRC in Emacs. It integrates well with the rest of the editor,
   ;; using standard Emacs key bindings and indicating activity in
   ;; channels in the status bar so it stays out of your way unless
   ;; you want to use it.
   (cons 'circe marmalade)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Load all .el files in private dir
(setq defuns-dir (expand-file-name "private" user-emacs-directory))
(dolist (file (directory-files defuns-dir t ".+\\.el$"))
  (when (file-regular-p file)
    (load file)))
(when (featurep 'private) (require 'private))

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
(eval-after-load 'iso-transl '(require 'setup-iso-transl))
(require 'setup-latex)
(require 'setup-hippie)
(require 'setup-perspective)
(require 'setup-ffip)
(require 'setup-html-mode)
(require 'scala-mode2)
(require 'setup-circe)
(require 'setup-geiser)
(require 'setup-prolog)
(require 'setup-haskell)
(require 'setup-java)

;; Map files to modes
(require 'mode-mappings)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'expand-region)
(require 'delsel)
(require 'jump-char)
(require 'eproject)
(require 'wgrep)
(require 'change-inner)

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

;; Email, baby
(require 'setup-mu4e)

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Emacs-Server.html
;; Emacs server
;; Emacs as an edit server, so that it “listens” for external edit
;; requests and share buffers, a command history, or other kinds of
;; information with any existing Emacs process
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
