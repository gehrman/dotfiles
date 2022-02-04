;;; config-flycheck --- Syntax-check all the things.

;;; Commentary:
;; None at this time.

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'flycheck
 )

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)
;; (setq flycheck-python-pycompile-executable "python3")

(setq byte-compile-warnings '(not free-vars))

(provide 'config-flycheck)
;;; config-flycheck.el ends here
