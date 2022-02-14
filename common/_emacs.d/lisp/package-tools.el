;;; config-package --- Manage packages

;;; Commentary:
;; My version of code to manage packages. I built most of this before before
;; I heard about use-package, and now the structure of my config is clean but
;; not the most amenable to conversion to use-package.

;;; Code:
(require 'package)

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

;; Unlike most of the other describes, describe-package doesn't prefill the minibuffer with the
;; symbol at point, if the symbol is a package. This goes too far - it just describes the symbol
;; there regardless, but it's a starting point.
(defun describe-package-at-point ()
  "Describe the package at point."
  (interactive)
  (describe-package (intern (thing-at-point 'symbol))))


(provide 'package-tools)
;;; package-tools ends here
