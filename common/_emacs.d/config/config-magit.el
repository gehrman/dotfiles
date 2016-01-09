;;; config-magit --- What it says on the tin.

;;; Commentary:
;; None at this time.

;;; Code:
;; Shut magit up.
(setq magit-last-seen-setup-instructions "1.4.0")

;; Bind C-k to set-mark-command. In magit, I can't seem to reliably stage blocks
;; by going into visual mode, C-SPC is launchers, and C-@ is crazy awkward. So,
;; I need another binding. C-k is a pneumonic for marK.
(global-set-key (kbd "C-k") 'set-mark-command)

(provide 'config-magit)
;;; config-magit.el ends here
