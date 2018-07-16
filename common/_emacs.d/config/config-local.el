;;; config-local --- OS & Host specific customizations.

;;; Commentary:
;; Rather spartan right now, but this will be necessary moving forward.
;;
;; For now, this will stay, but be a dispatch for site & os config files.
;; In particular, it seems like there's going to be a bunch of OS X stuff
;; that's better off on it's own.

;;; Code:
;; OS aware base directory.
(if (eq system-type 'windows-nt)
    (setq tramp-default-method "plink")
  (setq tramp-default-method "ssh"))

(if (eq system-type 'darwin)
    (require 'config-osx))

(provide 'config-local)
;;; config-local.el ends here
