;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: the package manager. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup package management.
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(defun ensure-package-installed (&rest packages)
  "Ensure every package listed is installed. If a packagge is not installed, try to install it.
   Returns a list of installed packages, or nil if every package is skipped."
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
                          ;'evil-escape
                          'magit
                          'anzu
                          'linum-relative
                          'flycheck
                          ;'flycheck-clojure
                          ;'flycheck-package
                          ;'flylisp
                          'powerline
                          'powerline-evil
                          'paredit
                          'helm)
;; File modes.
(ensure-package-installed 'puppet-mode)
;; Themes. In a different call, b/c they went rogue.
(ensure-package-installed 'ample-theme
                          'zenburn-theme
                          'warm-night-theme
                          'solarized-theme
                          'monokai-theme
                          'darcula-theme
                          )
(provide 'config-package)
