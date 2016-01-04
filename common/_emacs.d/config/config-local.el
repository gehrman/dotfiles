;;; config-local --- OS & Host specific customizations.

;;; Commentary:
;; Rather spartan right now, but this will be necessary moving forward.

;;; Code:
;; OS aware base directory.
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

(provide 'config-local)
;;; config-local.el ends here
