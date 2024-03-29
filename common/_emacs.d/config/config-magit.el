;;; config-magit --- What it says on the tin.

;;; Commentary:
;; None at this time.

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'magit
 )

(require 'evil)
(require 'magit)

;; Shut magit up.
(setq magit-last-seen-setup-instructions "1.4.0")

;; Make magit start full-frame, but otherwise behave the same.
;;(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

;; Allow multiple panes. Probably need to have a way to toggle these
;(setq magit-display-buffer-function 'magit-display-buffer-traditional)
(setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
;(setq magit-display-buffer-function 'magit-display-buffer)

;; Warn
(setq git-commit-summary-max-length 78)

;; Bind C-k to set-mark-command. In magit, I can't seem to reliably stage blocks
;; by going into visual mode, C-SPC is launchers, and C-@ is crazy awkward. So,
;; I need another binding. C-k is a pneumonic for marK.
;; TODO: Defer this
(define-key magit-mode-map (kbd "C-k") 'set-mark-command)
(define-key magit-mode-map (kbd "C-w") nil)
;; Enable leader key in magit
(define-key magit-mode-map (kbd ",") evil-leader--default-map)
;; (add-hook 'magit-mode-hook
;; (lambda ()
;;  (local-set-key (kbd "C-k") 'set-mark-command)))

;; (add-hook
;;  'magit-mode-hook
;;  (lambda ()
;;    (evil-define-key 'emacs magit-mode-map (kbd ", ,") 'ibuffer)
;;    (evil-define-key 'emacs magit-mode-map (kbd ", .") 'delete-other-windows)
;;    (evil-define-key 'emacs magit-mode-map (kbd ", w j") 'evil-window-left)
;;    (evil-define-key 'emacs magit-mode-map (kbd ", w l") 'evil-window-right)))

(defun magit-blame-start-or-quit ()
  "Start magit-blame if it's not running, quit if it is."
  (interactive)
  (let ((magit-blame-active (or magit-blame-mode magit-blame-read-only-mode)))
    (if magit-blame-active
        (magit-blame-quit)
      (magit-blame))))

;; Speed things up for monorepos / large tags repo
(remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

;; Open file in different worktree
;(find-file (concat "~/" "t"))
;(find-file (file))

;(buffer-name)
;(defun mfbfn () "Sentance." (interactive) (message (buffer-file-name)))
; (let ((worktree-prefix "vtrack/.*/vtrack"))
;  (message prefix))


;; (let ((file-name "/str/vtrack/name/vtrack/path/to/file")
;;       (worktree-prefix "vtrack/.*/vtrack"))
;;   (replace-regexp)
;;   (replace-string)
;;   (message prefix))

(provide 'config-magit)
;;; config-magit.el ends here
