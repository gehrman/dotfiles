;;; Package -- Summary
;;; Commentary:

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'lua-mode
 'yaml-mode
 'markdown-mode
 )

;; TODO: systematize insertion & navigation keybinds
(add-hook
 'markdown-mode-hook
 (lambda ()
   (local-set-key (kbd "s-f") 'markdown-insert-footnote)
   (local-set-key (kbd "s-h") 'markdown-insert-link)
   ))

(provide 'config-markup)
;;; config-markup ends here
