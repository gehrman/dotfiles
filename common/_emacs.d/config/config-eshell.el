;;; config-eshell -- EShell specific code.
;;; Commentary:
;; Holy crap, eshell is crazy nice.

;;; Code:
(defun read-only-p () "Is it?" (get-text-property (point) 'read-only))
(defun delete-backwards-to-readonly ()
  "Mimick the function of Ctrl-u in a terminal.

  This is only useful because we expect to be in emacs-state when in eshell,
  normally Vim bind make such a function unnecessary. This just deletes
  backwards until the text becomes read-only. There's probably a more efficient
  way of doing so - e.g. scanning backwards for the read-only transition, and
  then doing a single delete - but since this is basically only going to be used
  to clear eshell input lines, I'm not too concerned."
  (interactive)
  (while (not (read-only-p))
    (backward-delete-char 1)))

(add-hook
 'eshell-mode-hook
 (lambda () (define-key eshell-mode-map (kbd "C-u") 'delete-backwards-to-readonly)))

(defun start-or-kill-eshell ()
  "Fire up eshell, unless we're already in eshell, in which case kill it."
  (interactive)
  (if (equal major-mode 'eshell-mode)
      (kill-this-buffer)
    (eshell)))
(global-set-key (kbd "C-<return>") 'start-or-kill-eshell) ;; In restclient-mode this is shadowed into sending the request. I hope.

(provide 'config-eshell)
;;; config-eshell ends here
