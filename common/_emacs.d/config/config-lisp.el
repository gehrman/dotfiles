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

;;;
;; Clojure setup.
;;;

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
;;(add-hook 'clojure-mode-hook 'subword-mode)

;; syntax hilighting for midje
(add-hook
 'clojure-mode-hook
 (lambda ()
   (setq inferior-lisp-program "lein repl")
   (font-lock-add-keywords
    nil
    '(("(\\(facts?\\)"
       (1 font-lock-keyword-face))
      ("(\\(background?\\)"
       (1 font-lock-keyword-face))))
   (define-clojure-indent (fact 1))
   (define-clojure-indent (facts 1))))

;; Use clojure mode for other extensions
(mapc
 (lambda (mode-setting) (add-to-list 'auto-mode-alist mode-setting))
 '(("\\.edn$" . clojure-mode)
   ("\\.boot$" . clojure-mode)
   ("\\.cljs.*$" . clojure-mode)))
;; I guess lein uses ruby?
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(provide 'config-lisp)
;;; config-lisp.el ends here
