;;; gbe-writing -- Tools for writing

;;; Commentary:
;; deal will pollen, tex, markdown, etc

;;; Code:
(use-package pollen-mode
  :straight t

  :config
  (add-to-list 'auto-mode-alist '("\\.pp$" . pollen-mode)))

(provide 'gbe-writing)
;;; gbe-writing.el ends here
