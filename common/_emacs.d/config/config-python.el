;;; config-python --- Configuration specific to working with python code.
;;; Commentary:
;; References
;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/
;; http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/
;; http://www.emacswiki.org/emacs/ElDoc


;;; Code:
(require 'ein)

;; Use Jedi for Company auto-completions... need to pip install the following for
;; this to work:
;; jedi, epc, sexpdata
(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:environment-root "jedi")
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
;; From Patrick's config... python3 something
;; (setq jedi:environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "/usr/bin/python3")))

;; Make function definition is done with λ dammit. This makes me inordinately
;; happy.
(add-hook 'python-mode-hook
          (lambda ()
            (push '("lambda" . ?λ) prettify-symbols-alist)
            (push '("def" . ?λ) prettify-symbols-alist)
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)
            (push '("==" . ?≟) prettify-symbols-alist)
            (push '("sum" . ?Σ) prettify-symbols-alist)
            ; Greek Lowercase
            (push '("alpha" . ?α) prettify-symbols-alist)
            (push '("beta" . ?β) prettify-symbols-alist)
            (push '("gamma" . ?γ) prettify-symbols-alist)
            (push '("sigma" . ?σ) prettify-symbols-alist)
            ; Greek Uppercase
            (push '("Sigma" . ?Σ) prettify-symbols-alist)
            ))
;; Grabbed from Patrick's config... should try this some time.
; (eval-after-load "python"
;   '(define-key python-mode-map (kbd "<RET>") 'newline-and-indent))


(provide 'config-python)

;;; config-python.el ends here
