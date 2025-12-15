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
     (dolist (hook hook-list (remove-hook hook-list-name hook))))))

;; Unlike most of the other describes, describe-package doesn't prefill the minibuffer with the
;; symbol at point, if the symbol is a package. This goes too far - it just describes the symbol
;; there regardless, but it's a starting point.
(defun gbe/describe-package-at-point ()
  "Describe the package at point."
  (interactive)
  (let ((package-name (thing-at-point 'symbol t)))
    (describe-package (intern package-name))))

;(describe-package 'evil)
;(describe-package (intern "evil"))

(defun gbe/link-startup-files ()
  "Link Emacs dotfiles into .emacs.d for actual use.

  This works by calling the `sync-emacs-config` binary via \"call-process\" and
  output is written to the *config-synchronization* buffer."
  (interactive)

  ;; We need to make this work on multiple calls and display this somehow if
  ;; there were new contents written
  (let ((output-buffer-name "*config-synchronization*"))
    (get-buffer-create output-buffer-name)
    (set-buffer output-buffer-name)
    ;; Manually set buffer mode to help
    ;; (setq major-mode help-mode)
    (call-process "sync-emacs-config" nil t)))

(provide 'interactive-utilities)
;;; interactive-utilities.el ends here
