;;; gbe-keymaps -- Tools and global keymap setup

;;; Commentary:

;;; Code:
(define-prefix-command 'gbe/C-s-map)
(global-set-key (kbd "C-s") 'gbe/C-s-map)
(define-key gbe/C-s-map (kbd "C-s") 'isearch-forward)

(provide 'gbe-keymaps)
;;; gbe-keymaps.el ends here
