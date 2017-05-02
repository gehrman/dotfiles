;;; Package -- Summary
;;; Commentary:

;;; Code:
;; General setup for all lisp-y languages.
(autoload
  'enable-paredit-mode
  "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t
  )

(mapc
 (lambda (mode) (add-hook mode #'enable-paredit-mode))
 '(emacs-lisp-mode-hook
   eval-expression-minibuffer-setup-hook
   ielm-mode-hook
   clojure-mode-hook
   lisp-mode-hook
   lisp-interaction-mode-hook
   scheme-mode-hook
   ))

(mapc
 (lambda (mode) (add-hook mode 'turn-on-eldoc-mode))
 '(emacs-lisp-mode-hook
   lisp-interaction-mode-hook
   ielm-mode-hook
 ))

;; Clojure setup.


(provide 'config-lisp)
;;; config-lisp.el ends here
