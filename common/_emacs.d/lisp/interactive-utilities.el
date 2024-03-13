;;; interactive-utilities --- Summary
;; Basic emacs utilities

;;; Commentary:
;;

;;; Code:
(defun gbe/clear-hooks (&rest mode-hook)
  "Remove all functions from MODE-HOOK."
  (interactive
   (let* ((default-hook-list-name (and (symbolp (variable-at-point))
                                       (symbol-name (variable-at-point))))
          (hook-list-name (intern
                           (completing-read
                            (format-prompt "Mode hook" default-hook-list-name)
                            obarray #'boundp t nil nil default-hook-list-name)))
          (hook-list (symbol-value hook-list-name)))
     ;;(message (format "%s" (symbol-value hook-list-name)))
     ;;(message (format "list is %s" '(hook-list-name hook-list-name)))
     (dolist (hook hook-list (remove-hook hook-list-name hook)))
     ;;(dolist (hook hook-list (message (format "hook %s" (list hook)))))
     )))

;; Unlike most of the other describes, describe-package doesn't prefill the minibuffer with the
;; symbol at point, if the symbol is a package. This goes too far - it just describes the symbol
;; there regardless, but it's a starting point.
;; This never did work quite right... some day it would be nice to revisit this and get working
(defun gbe/describe-package-at-point ()
  "Describe the package at point."
  (interactive)
  (let ((package-name (thing-at-point 'symbol t)))
    (describe-package (intern package-name))))

;(describe-package 'evil)
;(describe-package (intern "evil"))

(defun gbe/link-startup-files ()
  "Link stuff. Replacement for the Emacs dotfile installer."
    (message "test"))

(provide 'interactive-utilities)
;;; interactive-utilities.el ends here
