;;; config-package --- Manage packages

;;; Commentary:
;;

;;; Code:
;; Setup package management.
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; Extra package repos that Clojure for the Brave and True configs default to
;; using.
;(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(defun ensure-package-installed (&rest packages)
  "Ensure every package in PACKAGES is installed.

  If a package is not installed, try to install it.  Returns a list of installed
  packages, or nil if every package is skipped."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
         (package-install package)
         package)))
   packages))

;; Make sure we have package data.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages.
(setq package-enable-at-startup nil)
(package-initialize)

;; Package groups.
;; Evil group first, as it's by far the most important.
(ensure-package-installed
 ;; A/K/A make emacs usable.
 'evil

 ;; Use the leader key.
 'evil-leader

 ;; Indentation text objects.
 'evil-indent-plus

 ;;'evil-anzu
 ;;'evil-magit ;some time I should try getting this to work again
 ;;'evil-lisp-state
 ;;'evil-cleverparens ; Another lisp mode.
 ;;'evil-commentary ; Another commenter.
 ;;'evil-args
 ;;'evil-escape
 ;;'evil-god-state ; What's god-mode?
 ;;'evil-iedit ; What's iedit?
 ;;'evil-mc ; Multiple cursors?
 ;;'evil-nerd-commenter
 ;;'evil-org ; This has been problematic.
 ;;'evil-visualstar
 )

;; Magit next, because, well, its amazing.
(ensure-package-installed
 'magit
 )

;; Productivity packages: autocomplete, snippets, and such
(ensure-package-installed
 'ag
 'company
 'hound
 ;;'sx ; search stackexchange et al
 ;;'wgrep ; What does this one actually do?
 'ibuffer-tramp
 'ibuffer-vc
 ;;'tramp-hdfs
 ;;'tramp-term
 ;;'tramp-theme
 )

;; General packages.
(ensure-package-installed
 'anzu
 'flycheck
 'helm ;Configure me!
 'helm-ag ;Configure me! (See https://github.com/syohex/emacs-helm-ag)
 'linum-relative
 'paredit
 'powerline
 'powerline-evil
 'visual-fill-column
 ;;'flycheck-clojure
 ;;'flycheck-package
 ;;'flylisp
 ;;'projectile
 ;;'helm-idris
 ;;'helm-projectile
 ;;'zeal-at-point ; zeal, or other documentation searching
 ;;'ibuffer-{tramp,*}
 ;;'tramp-hdfs
 ;;'tramp-term
 ;;'searchq
 ;;'zlc
 ;;'company-*
 ;;'znc
 ;;'sx
 ;;'sos
 ;;'yagist
 ;;'flx-*
 ;;'grizzl
 ;;'org-projectile, 'org-*
 ;;'svg-clock
 ;;'e2wm
 ;;'which-key-mode https://github.com/justbur/emacs-which-key
 )

;; Devops/infrastructure packages.
(ensure-package-installed
 'vagrant
 'vagrant-tramp
 ;;'docker
 ;;'dockerfile-mode
 ;;'docker-tramp
 )

;; Web development
(ensure-package-installed
 'restclient
 'restclient-helm
 'restclient-test
 'ob-http
 'ob-restclient
 'know-your-http-well
 'company-restclient
 ;;https://github.com/skeeto/skewer-mode
 ;; there's also swank-js + slime
 ;;'skewer-less
 ;;'skewer-mode
 ;;'skewer-reload-stylesheets
 )

;; Python
(ensure-package-installed
 'ein
 'company-jedi
 )

;; PHP
(ensure-package-installed
 'php-mode
 'php-refactor-mode
 'php-scratch
 'phpcbf
 'phpunit
 )

;; R and Statistics
(ensure-package-installed
 'ess
 )

;; Clojure & Lisp Development
(ensure-package-installed
 'clojure-mode
 'clojure-mode-extra-font-locking
 'cider
 'rainbow-delimiters
 )

;; Elm
(ensure-package-installed
 'elm-mode
 ;;'elm-yasnippets
 )

;; Rust
(ensure-package-installed
 'cargo
 ;;'flycheck-rust
 ;;'racer
 'rust-mode
 )

;; Latex
;; (ensure-package-installed
;;  'cdlatex
;;  'latex-math-preview
;;  'latex-pretty-symbols
;;  'latex-extra
;;  'latex-preview-pane
;;  'magic-latex-buffer
;;  'math-symbol-lists
;;  'px ; inline latex preview
;;  )

;; Misc file modes.
(ensure-package-installed
 'puppet-mode
 'idris-mode
 'lua-mode
 'yaml-mode
 'slime
 'scala-mode
 'ensime
 )

;; Applications
(ensure-package-installed
 'pianobar
 'rbt
 ;;'tomatinho ;https://github.com/konr/tomatinho
 )

;; Elisp Development
;; See https://github.com/Malabarba/names for namespace package stuff.
;; Magnar Sveen's dash and s are worthwhile here too. See
;; https://github.com/magnars/{dash.el,s.el}
;;(ensure-package-installed
;; 'names
;; 'dash
;; 's
;; )

;; UI Packages
(ensure-package-installed
 'transpose-frame
 'visible-mark
 'fill-column-indicator
 )

;; Themes.
(ensure-package-installed
 'ample-theme
 'zenburn-theme
 'warm-night-theme
 'solarized-theme
 'monokai-theme
 'darcula-theme
 'reykjavik-theme
 ;; 'afterglow - https://github.com/sjahl/emacs-theme-afterglow
 ;; 'afternoon-theme
 ;; 'noctilux-theme
 ;; 'grandshell-theme
 ;; 'alect-themes
 ;; 'cyberpunk-theme
 ;; 'tango-*
 ;; 'material-theme
 ;; 'aurora-theme
 )

(provide 'config-package)
;;; config-package ends here
