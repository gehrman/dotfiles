;;; Package -- Summary
;;; Commentary:

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'cargo
 ;;'flycheck-rust
 ;;'racer
 'rust-mode
 )

(provide 'config-rust)
;;; config-rust ends here
