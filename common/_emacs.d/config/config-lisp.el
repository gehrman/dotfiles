;;; Package -- Summary
;;; Commentary:

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'cider
 'clojure-mode
 'clojure-mode-extra-font-locking
 'elein
 ;;'flycheck-clojure
 'hy-mode
 'paredit
 'persistent-scratch
 'rainbow-delimiters
 'slime)

(require 'evil)
(require 'cider)

;; Elisp Development
;; See https://github.com/Malabarba/names for namespace package stuff.
;; Magnar Sveen's dash and s are worthwhile here too. See
;; https://github.com/magnars/{dash.el,s.el}
;;(ensure-package-installed
;; 'names
;; 'dash
;; 's
;; )

;; General setup for all lisp-y languages.
(autoload
  'enable-paredit-mode
  "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

;; Let's try saving scratch. Turned off because it's making *scratch* not load
;; telephone's mode line for some reason.
;;(persistent-scratch-autosave-mode)

(defun set-paredit-keys-for-mode ()
  "Setup keybinds for paredit."
  ;;(evil-define-minor-mode-key)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  ;; slurping
  (evil-define-minor-mode-key 'normal paredit-mode (kbd "C-h") 'paredit-backward-slurp-sexp)
  (evil-define-minor-mode-key 'normal paredit-mode (kbd "C-l") 'paredit-forward-slurp-sexp)
  (evil-define-minor-mode-key 'insert paredit-mode (kbd "C-h") 'paredit-backward-slurp-sexp)
  (evil-define-minor-mode-key 'insert paredit-mode (kbd "C-l") 'paredit-forward-slurp-sexp)
  (evil-define-minor-mode-key 'emacs paredit-mode (kbd "C-h") 'paredit-backward-slurp-sexp)
  (evil-define-minor-mode-key 'emacs paredit-mode (kbd "C-l") 'paredit-forward-slurp-sexp)
  ;; Barfing
  (evil-define-minor-mode-key 'normal paredit-mode (kbd "M-h") 'paredit-backward-barf-sexp)
  (evil-define-minor-mode-key 'normal paredit-mode (kbd "M-l") 'paredit-forward-barf-sexp)
  (evil-define-minor-mode-key 'insert paredit-mode (kbd "M-h") 'paredit-backward-barf-sexp)
  (evil-define-minor-mode-key 'insert paredit-mode (kbd "M-l") 'paredit-forward-barf-sexp)
  (evil-define-minor-mode-key 'emacs paredit-mode (kbd "M-h") 'paredit-backward-barf-sexp)
  (evil-define-minor-mode-key 'emacs paredit-mode (kbd "M-l") 'paredit-forward-barf-sexp)
  ;; Splicing feels like barfing, so it's under a meta bind
  (evil-define-minor-mode-key 'normal paredit-mode (kbd "M-e") 'paredit-splice-sexp)
  )
(add-hook 'paredit-mode-hook 'set-paredit-keys-for-mode)

(mapc
 (lambda (mode)
   (add-hook mode #'enable-paredit-mode))
 '(emacs-lisp-mode-hook
   set-paredit-keys-for-mode
   eval-expression-minibuffer-setup-hook
   clojure-mode-hook
   hy-mode-hook
   ielm-mode-hook
   lisp-mode-hook
   lisp-interaction-mode-hook
   scheme-mode-hook))

(mapc
 (lambda (mode) (add-hook mode 'turn-on-eldoc-mode))
 '(emacs-lisp-mode-hook
   ielm-mode-hook
   hy-mode-hook
   lisp-interaction-mode-hook))

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

;; In IElm, since we have paredit running, <return> won't evaluate code as it is
;; instead running paredit-ret. That's not the worst - it sorta works out like
;; ipython, where we can write multiline literals. That said, we need to bind
;; ielm evaluation. Since there's not really a need to pop into eshell from ielm
;; we'll use C-ret
;; This needs to be a mode-hook so that it fire after ielm-map exists
;(define-key ielm-map (kbd "C-<return>") 'ielm-send-input)


(provide 'config-lisp)
;;; config-lisp.el ends here
