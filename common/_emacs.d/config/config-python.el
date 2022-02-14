;;; config-python --- Configuration specific to working with python code.
;;; Commentary:
;; References
;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/
;; http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/
;; http://www.emacswiki.org/emacs/ElDoc


;;; Code:
(require 'package-tools)

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
 'pyenv-mode
 'pyenv-mode-auto
 )

(require 'ein)
(require 'pytest)

;; Black-on-save, with style. And some insanity.
(defun maybe-blacken-buffer ()
  "Run blacken-buffer on save if the buffer is not narrowed.

   This is useful because running black on a narrowed buffer will break
   the narrowing. The checking major mode thing is a hack, and really
   this all needs to be upstreamed to blacken."
  (interactive)
  (if (buffer-narrowed-p)
      (send-notification-from-emacs "Buffer is narrowed, we are NOT running black.")
      (when (equal major-mode 'python-mode)
        (blacken-buffer))))

;; We're about to do something _really_ gross to get around the problem of
;; 'save-buffer widening before it runs 'before-save-hook. Specifically, we're
;; going to capture the content of the 'save-buffer function into another
;; function name. Then we're going to re-write save-buffer to first run our
;; custom hook, then run the content of the builtin save-buffer function. All
;; this to avoid the save-buffer function running the 'before-save-hook _after_
;; the buffer has been widened for saving. Yes, this is _insane_.
(defvar really-before-save-hook nil
  "A hook to be run _before_ any 'save-buffer code runs, not in the middle of the saving code.")
;; (add-hook 'really-before-save-hook 'maybe-blacken-buffer)
(fset 'builtin-save-buffer
      (symbol-function 'save-buffer))
(defun save-buffer (&optional arg)
  "My 'save-buffer function. ARG doesn't do anything anymore."
  (run-hooks 'really-before-save-hook)
  (builtin-save-buffer))

;; Use auto-pyenv
(require 'pyenv-mode)
(require 'pyenv-mode-auto)
(add-hook 'python-mode-hook 'pyenv-mode)
;; Use Jedi for Company auto-completions... need to pip install the following for
;; this to work:
;; jedi, epc, sexpdata
;; (require 'company-jedi)
;; (add-to-list 'company-backends 'company-jedi)
;;
;; (add-hook 'python-mode-hook 'jedi:setup)
;;(jedi:install-server)
;;(setq jedi:environment-root "jedi")
;; (setq jedi:environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "/usr/local/bin/python3")))
;; (setq py-python-command "/usr/local/bin/python3")
;;(setq jedi:setup-keys t)
;;(setq jedi:complete-on-dot t)
;;(setq jedi:get-in-function-call-delay 10000) ; 10s delay before showing function call sigs

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

(defun insert-env-stuff ()
  "Create an env file."
  (interactive)
  (insert "CMT_HOME=" default-directory "\n"))
(defun insert-pdbpp-install-stuff ()
  "Create a pdb++ install script."
  (interactive)
  (insert "#! /bin/sh\ncd appserver && pipenv run pip install pdbpp && cd ..\n"))

(defun new-env-file ()
  "Create a real env file."
  (interactive)
  ;; Set up .env
  (find-file ".env")
  (insert "CMT_HOME=" default-directory "\n")
  (write-file ".env")
  (kill-buffer)

  ;; Set up pdbpp installer
  (cd "vtrack")
  (find-file "gimme-setup")
  (insert "#! /bin/sh\nbash docker/common_test_setup.sh\ncd appserver && pipenv run pip install pdbpp && cd ..\n")
  (write-file "gimme-setup")
  (kill-buffer)
  (chmod "gimme-setup" #o755)

  ;; Set up test script
  (find-file "run-tests")
  (insert "#! /bin/sh\n\nbash docker/run_test_step_vtrackserver.sh\n")
  (write-file "run-tests")
  (kill-buffer)
  (chmod "run-tests" #o755)

  (cd "..")
  (revert-buffer))

(add-hook 'dired-mode-hook (lambda ())
          (evil-leader/set-key "e" 'new-env-file))

;; TODO: Make this non-global
(defun insert-pdb-breakpoint ()
   "Insert a pdb break."
   (interactive)
   (save-excursion
     (insert "breakpoint()")))

(defun insert-ipdb-breakpoint ()
   "Insert a pdb set_trace. Only necessary for pre-Python 3.7 code."
   (interactive)
   (save-excursion
     (insert "import pdb; pdb.set_trace()")))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "M-i") 'insert-pdb-breakpoint)
            (local-set-key (kbd "M-i") 'insert-pdb-breakpoint)))

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
   ;(push '("async def" . '(?# (α . λ) ? ())) prettify-symbols-alist)  ;; This can be done, but need to learn the syntax/install the prettify-utils pkg
   (push '("async" . ?α) prettify-symbols-alist)
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
