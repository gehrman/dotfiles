;; bootstrap --- Bare-minimum startup configuration

;;; Commentary:
;; Basically, this file exists to set up load paths and package infrastructure

;;; Code:

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "config"))
(add-to-list 'load-path (locate-user-emacs-file "package-config"))

;; Extra Repos
;; Tromey includes extra package repos that Clojure for the Brave and True
;; configs default to using.
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)

;; Set up straight.el and use-package.el. We want to get these installed first
;; as we'll be using them to set everything else up. (Eventually.) Note that
;; once things are converted, we can disable package.el in early-init
;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(unless (package-installed-p 'use-package)
  (straight-use-package 'use-package))

;; Original installer function, moved here to streamline structure. This should
;; eventually be removable, once the all configs are migrated to package-config
(defun ensure-package-installed (&rest packages)
  "Ensure every package in PACKAGES is installed.

  THIS IS DEPRECATED. NEW INSTALLS AND CONFIGS SHOULD USE STRAIGHT AND
  USE-PACKAGE.

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


(provide 'bootstrap)
;;; bootstrap.el ends here.
