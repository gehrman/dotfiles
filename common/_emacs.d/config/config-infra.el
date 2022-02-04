;;; Package -- Summary

;;; Commentary:
;;; Infrastructure settings for things like puppet, terraform, ansible, etc

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'company-terraform
 'terraform-doc
 'terraform-mode)

(defun maybe-terraform-format ()
  "Run terraform-format-buffer if we're in the correct 'major-mode'."
  (interactive)
  (when (equal major-mode 'terraform-mode)
    (terraform-format-buffer)))
(add-hook 'before-save-hook 'maybe-terraform-format)

(provide 'config-infra)
;;; config-infra ends here
