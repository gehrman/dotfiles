;;; Package -- Summary
;;; Commentary:

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'php-mode
 'php-refactor-mode
 'php-scratch
 'phpcbf
 'phpunit
 )

(provide 'config-php)
;;; config-php ends here
