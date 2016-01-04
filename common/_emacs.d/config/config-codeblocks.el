;;; config-codeblocks --- Setup org-mode code blocks.

;;; Commentary:

;;; Code:
;; Set languages available for execution in code blocks.
(require 'cl) ; We need the common lisp package for 'remove-duplicates.
(org-babel-do-load-languages
  'org-babel-load-languages
  (remove-duplicates (append org-babel-load-languages
                             '((emacs-lisp . t)(sh . t)(python . t)))
                     :test 'equal))

(provide 'config-codeblocks)
;;; config-codeblocks.el ends here
