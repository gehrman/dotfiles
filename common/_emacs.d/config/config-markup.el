;;; Package -- Summary
;;; Commentary:

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'lua-mode
 'yaml-mode
 'markdown-mode
 'easy-jekyll
 ;; Markdown preview stuff
 ;'gh-md
 'grip-mode
 'markdown-preview-mode
 ;'markdown-soma
 ;'mkdown
 ;; wtf is this??
 ;; impatient-mode
 ;; impatient-showdown
 )

;; TODO: systematize insertion & navigation keybinds
(add-hook
 'markdown-mode-hook
 (lambda ()
   (define-key markdown-mode-map (kbd "s-f") 'markdown-insert-footnote)
   (define-key markdown-mode-map (kbd "s-h") 'markdown-insert-link)
   (define-key markdown-mode-map (kbd "C-j") 'grip-start-preview)
   (define-key markdown-mode-map (kbd "C-k") 'grip-browse-preview)
   ))


(provide 'config-markup)
;;; config-markup ends here
