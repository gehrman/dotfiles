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

(defun set-paredit-keys-for-mode ()
  "Setup keybinds for paredit."
  ;;(evil-define-minor-mode-key)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-define-minor-mode-key 'normal paredit-mode (kbd "C-h") 'paredit-backward-barf-sexp)
  (evil-define-minor-mode-key 'normal paredit-mode (kbd "C-k") 'paredit-forward-slurp-sexp)
  (evil-define-minor-mode-key 'normal paredit-mode (kbd "C-l") 'paredit-forward-barf-sexp)
  )
(add-hook 'paredit-mode-hook 'set-paredit-keys-for-mode)

(mapc
 (lambda (mode)
   (add-hook mode #'enable-paredit-mode))
 '(emacs-lisp-mode-hook
   set-paredit-keys-for-mode
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

;;;
;; Cider Setup
;;;

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect t)
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; History Settings
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-wrap-history t)

;; Use Paredit in the REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Helper functions for working with Cider.
(defun cider-start-http-server ()
  "Start the Cider HTTP server."
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  "Reset Cider."
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  "Set the repl namespace."
  (interactive)
  (cider-repl-set-ns "user"))

;; Keybindings
;;(eval-after-load 'cider
;;  '(progn
;;     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
;;     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
;;     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
;;     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

(provide 'config-lisp)
;;; config-lisp.el ends here
