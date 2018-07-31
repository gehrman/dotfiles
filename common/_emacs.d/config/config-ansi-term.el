;;; Package -- Summary
;;; Commentary:

;;; Code:
;; TODO: make this mode-specific
;; TODO: manage states --
;;       line-mode => evil-emacs-state + move point to right place
;;       char-mode => evil-normal-state
(global-set-key (kbd "s-o") 'term-line-mode)
(global-set-key (kbd "s-p") 'term-char-mode)

(provide 'config-ansi-term)
;;; config-ansi-term ends here
