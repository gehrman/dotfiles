;;; config-magit --- What it says on the tin.

;;; Commentary:
;; None at this time.

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'magit
 )

(require 'evil)

;; Shut magit up.
(setq magit-last-seen-setup-instructions "1.4.0")

;; Make magit start full-frame, but otherwise behave the same.
(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

;; Warn
(setq git-commit-summary-max-length 50)

;; Bind C-k to set-mark-command. In magit, I can't seem to reliably stage blocks
;; by going into visual mode, C-SPC is launchers, and C-@ is crazy awkward. So,
;; I need another binding. C-k is a pneumonic for marK.
(global-set-key (kbd "C-k") 'set-mark-command)

(add-hook
 'magit-mode-hook
 (lambda ()
   (evil-define-key 'emacs magit-mode-map (kbd ", ,") 'ibuffer)
   (evil-define-key 'emacs magit-mode-map (kbd ", .") 'delete-other-windows)
   (evil-define-key 'emacs magit-mode-map (kbd ", w j") 'evil-window-left)
   (evil-define-key 'emacs magit-mode-map (kbd ", w l") 'evil-window-right)
   ))

(provide 'config-magit)
;;; config-magit.el ends here
