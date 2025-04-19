;;; config-remote -- Config for dealing with remote systems and TRAMP
;;; Commentary:
;;

;;; Code:
(setq tramp-remote-path '("/usr/local/bin" "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/sbin"))
(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")

(provide 'config-remote)
;;; config-remote ends here
