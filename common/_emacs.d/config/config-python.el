;;; config-python --- Configuration specific to working with python code.
;;; Commentary:
;; References
;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/
;; http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/
;; http://www.emacswiki.org/emacs/ElDoc


;;; Code:
(ensure-package-installed
 'cython-mode
 'blacken
 'ein
 ;;'elpy
 'company-jedi
 'virtualenvwrapper
 ;;'python-docstring
 'python-switch-quotes
 'pytest ;;--- This doesn't seem to play well with tramp, so need to look at alternatives
 ;; syntax checking - flycheck(make?)
 )

(require 'ein)

;; Black-on-save
(add-hook 'python-mode-hook 'blacken-mode)

;; Use Jedi for Company auto-completions... need to pip install the following for
;; this to work:
;; jedi, epc, sexpdata
(require 'company-jedi)
(add-to-list 'company-backends 'company-jedi)

(add-hook 'python-mode-hook 'jedi:setup)
;;(jedi:install-server)
(setq jedi:environment-root "jedi")
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 10000) ; 10s delay before showing function call sigs

;; Virtualenv tools
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
;; (venv-workon)
;; (venv-deactivate)
;; (venv-mkvirtualenv)
;; (venv-allvirtualenv)
;; (venv-allvirtualenv-shell-command)
;; Auto-activate w/ projectile... learn projectile
;; (setq projectile-switch-project-action
;;       '(lambda ()
;;          (venv-projectile-auto-workon)
;;          (projectile-find-file)))
;; (setq venv-dirlookup-names '(".venv" "pyenv" ".virtual"))


;; Utility functions
(require 'python-switch-quotes)

;; Enable code-folding by default
(add-hook 'python-mode-hook 'hs-minor-mode)

(defun insert-dunder-main ()
  "Insert a python main block."
  (interactive)
  (insert "import argparse\nif __name__ == '__main__':\n    "))

;; TODO: Make this non-global
(global-set-key
 (kbd "s-i")
 (defun insert-pdb-breakpoint ()
   "Insert a pdb break."
   (interactive)
   (save-excursion
     (insert "import pdb; pdb.set_trace()")
     )))

(global-set-key
 (kbd "s-I")
 (defun insert-ipdb-breakpoint ()
   "Insert a pdb break."
   (interactive)
   (save-excursion
     (insert "import ipdb; ipdb.set_trace()")
     )))

;; From Patrick's config... python3 something
;; (setq jedi:environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "/usr/bin/python3")))

;; Test Integration

;; TODO:
;;   - add save file hook correctly
;;   - keybindings, also (https://github.com/company-mode/company-mode/issues/75)

;; We want to modify pytest-module (defined in pytest.el) so that it runs the
;; "right" test, based on the file name, rather than the file itself. The function
;; is listed below.
;;
;;(defun pytest-module (&optional flags)
;;  "Run pytest (via eggs/bin/test) on current buffer.
;;Optional argument FLAGS py.test command line flags."
;;  (interactive)
;;  (pytest-run buffer-file-name flags))
;;
;; I think the strategy here is to replace 'buffer-file-name with a fuction, call
;; it 'pytest-get-test-from-buffer-name. The logic should be something like:
;;   - if (buffer-file-name) starts with "test_" run that
;;   - if not, but (join "test_" buffer-files-name) exists, run that
;;   - (maybe?) if not, but (join "test_" buffer-files-name) is a buffer, run that
;;   - if not, walk upward looking for a dir with a tests dir in it, then do the
;;     corresponding walk down and look for (join "test_" buffer-files-name)
;;   - if that fails, quit with "Don't know how to run tests for this buffer.
;;
;; ...I think that's the right and proper resolution, but notice that checking if
;; buffer is a test and looking for a corresponding test buffer if it's not gets
;; you 80% there. With 'get-buffer you'll either get 'nil, it's not open, or the
;; name as a string.
(defun my-get-test-buffer-name()
  "This."
  (interactive)
  (cond
   ((string-prefix-p "test" (buffer-name))
    (buffer-name))
   ((get-buffer (concat "test_" (buffer-name)))
    (concat "test_" (buffer-name)))
   ((get-buffer (concat "test" (buffer-name)))
    (concat "test" (buffer-name)))))


(defun my-try-to-run-pytest-from-buffer-name (&optional flags)
  "Run with FLAGS."
  (interactive)
  (when (my-get-test-buffer-name)
    (pytest-run
     (buffer-file-name (get-buffer (my-get-test-buffer-name)))
     flags)))

(defun my-try-to-run-pytest-from-buffer-name () "No." (lambda nil)) ;; Stahp.
(defun run-pytest-from-buffer-name-on-save ()
  "Run test on save."
  (when (eq major-mode 'python-mode)
    (my-try-to-run-pytest-from-buffer-name)))
;;(add-hook 'after-save-hook #'run-pytest-from-buffer-name-on-save)
;; So that hook's not ready yet.

;; Make function definition is done with λ dammit. This makes me inordinately
;; happy.
(add-hook
 'python-mode-hook
 (lambda ()
   (push '("def" . ?λ) prettify-symbols-alist)
   (push '("class" . ?Λ) prettify-symbols-alist)
   (push '("sum" . ?Σ) prettify-symbols-alist)
   ;; Greek Lowercase
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
   ;; Greek Uppercase
   (push '("Gamma" . ?Γ) prettify-symbols-alist)
   (push '("Delta" . ?Δ) prettify-symbols-alist)
   (push '("Lambda" . ?Λ) prettify-symbols-alist)
   (push '("Pi" . ?Π) prettify-symbols-alist)
   (push '("Sigma" . ?Σ) prettify-symbols-alist)
   (push '("Phi" . ?Φ) prettify-symbols-alist)
   (push '("Psi" . ?Ψ) prettify-symbols-alist)
   (push '("Omega" . ?Ω) prettify-symbols-alist)))
;; Grabbed from Patrick's config... should try this some time.
; (eval-after-load "python"
;   '(define-key python-mode-map (kbd "<RET>") 'newline-and-indent))

(provide 'config-python)

;;; config-python.el ends here
