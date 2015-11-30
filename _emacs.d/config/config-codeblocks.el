;; Set languages available for execution in code blocks.
(org-babel-do-load-languages
  'org-babel-load-languages
  (remove-duplicates (append org-babel-load-languages
                             '((emacs-lisp . t)(sh . t)(python . t)))
                     :test 'equal))
(provide 'config-codeblocks)
