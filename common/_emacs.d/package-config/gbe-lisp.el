;;; gbe-lisp --- configuration for lisp work

;;; Commentary:

;;; Code:
;; This should use-package sly
(use-package hy-mode
  :straight (:host github :repo "hylang/hy-mode")
  ;; (add-hook 'hy-mode-hook 'paredit-mode)
  ;; may also be able to turn on eldoc support
  :hook (hy-mode . paredit-mode))

(use-package racket-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode)))

(provide 'gbe-lisp)
;;; gbe-lisp.el ends here
