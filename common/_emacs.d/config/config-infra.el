;;; Package -- Summary
;;; Commentary: Infrastructure settings for things like puppet, terraform, ansible, etc

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'company-terraform
 'terraform-doc
 'terraform-mode)

(provide 'config-infra)
;;; config-infra ends here
