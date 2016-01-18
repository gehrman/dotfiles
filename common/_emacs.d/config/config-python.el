;;; config-python --- Configuration specific to working with python code.
;;; Commentary:
;; References
;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/
;; http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/
;; http://www.emacswiki.org/emacs/ElDoc


;;; Code:

;; Make function definition is done with λ dammit. This makes me inordinately
;; happy.
(add-hook 'python-mode-hook
          (lambda ()
            (push '("lambda" . ?λ) prettify-symbols-alist)
            (push '("def" . ?λ) prettify-symbols-alist)
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)))

(provide 'config-python)

;;; config-python.el ends here
