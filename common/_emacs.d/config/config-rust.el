;;; Package -- Summary
;;; Commentary:

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'cargo
 ;;'flycheck-rust
 'racer
 'rust-mode
 )

(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; Rust-specific leader binds
(evil-leader/set-key-for-mode 'rust-mode
  "<tab>" 'company-indent-or-complete-common
  "." 'rust-format-buffer
  "c" 'rust-compile)

;; Auto-format on save
(add-hook 'before-save-hook 'rust-format-buffer)

;; Racer setup
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)


(provide 'config-rust)
;;; config-rust ends here
