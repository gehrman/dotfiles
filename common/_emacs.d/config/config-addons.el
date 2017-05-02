;;; Package -- Summary
;;; Commentary:
;;; Dumping ground for interactive functions, and things like that.
;;; Maybe extensions is a better name? Must ponder.
;;; Code:

;; This should really be region specific.
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
;(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs ()
  "Untabify a buffer."
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(provide 'config-addons)
;;; config-addons ends here
