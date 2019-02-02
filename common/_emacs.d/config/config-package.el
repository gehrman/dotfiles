;;; config-package --- Manage packages

;;; Commentary:
;;

;;; Code:
;; Setup package management.
(require 'package)
(require 'package-tools)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; Extra package repos that Clojure for the Brave and True configs default to
;; using.
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)


;; Make sure we have package data.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages.
(setq package-enable-at-startup nil)
(package-initialize)

;; Productivity packages: autocomplete, snippets, and such
(ensure-package-installed
 'ag
 'hound
 ;;'sos/'sx ; search stackexchange et al
 ;;'wgrep ; What does this one actually do?
 'ibuffer-tramp
 'ibuffer-vc
 ;;thingatpt/thingatpt+
 ;;'tramp-hdfs
 ;;'tramp-term
 ;;'tramp-theme
 )

;; General packages.
(ensure-package-installed
 'anzu
 ;; Currently using IDO & company for completions, not helm
 'browse-kill-ring
 ;;'flycheck-package
 ;;'flx-*
 ;;'flylisp
 ;;'helm-idris
 ;;'helm-projectile
 ;;'zeal-at-point ; zeal, or other documentation searching
 ;;'zlc
 ;;'org-projectile, 'org-*
 ;;'svg-clock
 ;;'e2wm
 ;;'which-key-mode https://github.com/justbur/emacs-which-key
 'paradox
 'protobuf-mode
 'puppet-mode
 )

;; Applications
(ensure-package-installed
 'rbt
 'daemons
 ;;'tomatinho ;https://github.com/konr/tomatinho
 )

(provide 'config-package)
;;; config-package ends here
