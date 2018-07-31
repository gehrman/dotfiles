;;; config-package --- Manage packages

;;; Commentary:
;;

;;; Code:
;; Setup package management.
(require 'package)
(package-install 'use-package)
(require 'use-package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; Extra package repos that Clojure for the Brave and True configs default to
;; using.
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Unlike most of the other describes, describe-package doesn't prefill the minibuffer with the
;; symbol at point, if the symbol is a package. This goes too far - it just describes the symbol
;; there regardless, but it's a starting point.
(defun describe-package-at-point ()
  "Describe the package at point."
  (interactive)
  (describe-package (intern (thing-at-point 'symbol))))

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

;; Productivity packages: autocomplete, snippets, and such
(ensure-package-installed
 'ag
 'company
 'company-flx
 'hound
 'projectile
 ;;'sx ; search stackexchange et al
 ;;'wgrep ; What does this one actually do?
 'ido-ubiquitous
 'ibuffer-tramp
 'ibuffer-vc
 'smex
 ;;thingatpt/thingatpt+
 ;;'tramp-hdfs
 ;;'tramp-term
 ;;'tramp-theme
 )

;; General packages.
(ensure-package-installed
 'anzu
 ;; Currently using IDO & company for completions, not helm
 ;;'helm ;Configure me!
 ;;'helm-ag ;Configure me! (See https://github.com/syohex/emacs-helm-ag)
 'linum-relative
 'powerline
 'powerline-evil
 'visual-fill-column
 'browse-kill-ring
 'exec-path-from-shell
 ;;'flycheck-package
 ;;'flx-*
 ;;'flylisp
 ;;'helm-idris
 ;;'helm-projectile
 ;;'zeal-at-point ; zeal, or other documentation searching
 ;;'searchq
 ;;'zlc
 ;;'znc
 ;;'sos
 ;;'yagist
 ;;'grizzl
 ;;'org-projectile, 'org-*
 ;;'svg-clock
 ;;'e2wm
 ;;'which-key-mode https://github.com/justbur/emacs-which-key
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

;; Rust
(ensure-package-installed
 'cargo
 ;;'flycheck-rust
 ;;'racer
 'rust-mode
 )

;; Misc file modes.
(ensure-package-installed
 'puppet-mode
 'lua-mode
 'yaml-mode
 'markdown-mode
 )

;; Applications
(ensure-package-installed
 'pianobar
 'rbt
 'daemons
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
 'paradox
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
