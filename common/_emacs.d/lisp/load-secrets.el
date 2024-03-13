;;; Package -- Summary
;;; Commentary:
;; This package defines functions for loading secrets data into Emacs. The
;; intended usage is
;; (setq my-secret-variable (load-secrets/load-string "path/to/secret")).
;; That it, it can keep secrets out of your Emacs config (and thus more
;; easily gitignore-able) but doesn't handle encryption. The intent is to
;; keep secrets from accidently showing up on Github, not hardened to a
;; attacker with access to your file-system.

;;; Code:
(defun load-secret/load-string (path)
  "Load the secret stored at PATH."
  ())

(provide 'load-secrets)
;;; load-secrets.el ends here
