;;; config-orgmode -- Setup org-mode.

;;; Commentary:

;;; Code:
;; Set languages available for execution in code blocks.
(require 'cl) ; We need the common lisp package for 'remove-duplicates.
(org-babel-do-load-languages
  'org-babel-load-languages
  (remove-duplicates (append org-babel-load-languages
                             '((emacs-lisp . t)(sh . t)(python . t)))
                     :test 'equal))

(provide 'config-orgmode)
;;; config-orgmode.el ends here
