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
(setq jedi:get-in-function-call-delay 10000) ; 10s delay before showing function call sigs

(defun insert-dunder-main ()
  "Insert a python main block."
  (interactive)
  (insert "import argparse\nif __name__ == '__main__':\n    "))

;; From Patrick's config... python3 something
;; (setq jedi:environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "/usr/bin/python3")))

;; Make function definition is done with λ dammit. This makes me inordinately
;; happy.
(add-hook 'python-mode-hook
          (lambda ()
            (push '("def" . ?λ) prettify-symbols-alist)
            (push '("sum" . ?Σ) prettify-symbols-alist)
            ; Greek Lowercase
            (push '("alpha" . ?α) prettify-symbols-alist)
            (push '("beta" . ?β) prettify-symbols-alist)
            (push '("gamma" . ?γ) prettify-symbols-alist)
            (push '("delta" . ?δ) prettify-symbols-alist)
            (push '("epsilon" . ?ε) prettify-symbols-alist)
            (push '("zeta" . ?ζ) prettify-symbols-alist)
            (push '("eta" . ?η) prettify-symbols-alist)
            (push '("theta" . ?θ) prettify-symbols-alist)
            (push '("iota" . ?ι) prettify-symbols-alist)
            (push '("kappa" . ?κ) prettify-symbols-alist)
            (push '("lambda" . ?λ) prettify-symbols-alist)
            (push '("mu" . ?μ) prettify-symbols-alist)
            (push '("pi" . ?π) prettify-symbols-alist)
            (push '("rho" . ?ρ) prettify-symbols-alist)
            (push '("sigma" . ?σ) prettify-symbols-alist)
            (push '("tau" . ?τ) prettify-symbols-alist)
            (push '("phi" . ?φ) prettify-symbols-alist)
            (push '("chi" . ?χ) prettify-symbols-alist)
            (push '("psi" . ?ψ) prettify-symbols-alist)
            (push '("omega" . ?ω) prettify-symbols-alist)
            ; Greek Uppercase
            (push '("Gamma" . ?Γ) prettify-symbols-alist)
            (push '("Delta" . ?Δ) prettify-symbols-alist)
            (push '("Lambda" . ?Λ) prettify-symbols-alist)
            (push '("Pi" . ?Π) prettify-symbols-alist)
            (push '("Sigma" . ?Σ) prettify-symbols-alist)
            (push '("Phi" . ?Φ) prettify-symbols-alist)
            (push '("Psi" . ?Ψ) prettify-symbols-alist)
            (push '("Omega" . ?Ω) prettify-symbols-alist)
            ))
;; Grabbed from Patrick's config... should try this some time.
; (eval-after-load "python"
;   '(define-key python-mode-map (kbd "<RET>") 'newline-and-indent))

(provide 'config-python)

;;; config-python.el ends here
