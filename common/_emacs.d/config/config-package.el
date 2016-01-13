;;; config-package --- Manage packages

;;; Commentary:
;;

;;; Code:
;; Setup package management.
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(defun ensure-package-installed (&rest packages)
  "Ensure every package in PACKAGES is installed.

  If a package is not installed, try to install it. Returns a list of installed
  packages, or nil if every package is skipped."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
         (package-install package)
         package)))
   packages))

;; Make sure we have package data.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages.
(setq package-enable-at-startup nil)
(package-initialize)

;; Packages to use, in rough order of descending importance.
;flycheck python go go go
(ensure-package-installed 'evil
                          'evil-leader
                          'evil-escape ; Escape from anything.
                          'evil-lisp-state
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
                          'magit
                          ;;'evil-magit
                          'anzu
                          ;;'evil-anzu
                          'linum-relative
                          'flycheck
                          ;'flycheck-clojure
                          ;'flycheck-package
                          ;'flylisp
                          'powerline
                          'powerline-evil
                          'paredit
                          ;;'pt ; this one seems to suck/not work on windows
                          'helm
                          'helm-ag
                          ;;'wgrep ; What does this one actually do?
                          ;;'helm-pt
                          ;;'projectile
                          ;;'helm-projectile
                          ;;'sx ; search stackexchange et al
                          ;;'zeal-at-point ; zeal, or other documentation searching
                          ;;'docker
                          ;;'dockerfile-mode
                          ;;'docker-tramp
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
                          ;;'idris-mode
                          ;;'helm-idris
                          ;;'org-projectile, 'org-*
                          'vagrant
                          ;;'vagrant-tramp
                          ;; Latex Packages
                          'cdlatex
                          ;;'latex-extra
                          'latex-math-preview
                          'latex-pretty-symbols
                          ;;'latex-preview-pane
                          ;;'magic-latex-buffer
                          ;;'math-symbol-lists
                          ;;'px ; inline latex preview
                          ;; More stuff.
                          )
;; File modes.
(ensure-package-installed 'puppet-mode)
;; Themes. In a different call, b/c they went rogue.
(ensure-package-installed 'ample-theme
                          'zenburn-theme
                          'warm-night-theme
                          'solarized-theme
                          'monokai-theme
                          'darcula-theme
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
