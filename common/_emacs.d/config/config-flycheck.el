;;; config-flycheck --- Syntax-check all the things.

;;; Commentary:
;; None at this time.

;;; Code:
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)

(provide 'config-flycheck)
;;; config-flycheck.el ends here
