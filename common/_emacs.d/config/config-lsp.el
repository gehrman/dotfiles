;;; Package -- Summary
;;; Commentary:
;; General lsp setup. Language-specific code should eventually all move to the
;; language specific files.
;;
;; So far this is all rust-specific, and is basically taken from
;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/

;;; Code:

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; todo - add this to use-package when stable... also get this working with minor
;; modes
;; (evil-leader/set-key-for-mode 'lsp-mode
;;   "." 'lsp-find-definition
;;   "?" 'lsp-find-references
;;   "/" 'xref-pop-marker-stack
;;   "lj" 'lsp-ui-imenu
;;   )

;; (defvar lsp-mode-leader-map (make-sparse-keymap)
;;   "Keymap for \"leader key\" shortcuts in the lsp minor mode.
;; This is required because evil-leader/set-key-for-mode only works
;; with major modes.")
;; (set-keymap-parent lsp-mode-leader-map evil-leader--default-map)
;; (define-key lsp-mode-map "," lsp-mode-leader-map)
;; (define-key lsp-mode-leader-map "?" 'lsp-find-references)
;; (define-key lsp-mode-leader-map "$" 'lsp-find-references)
;; (define-key lsp-mode-leader-map "%" 'lsp-find-references)


;; Per Kra.hn, lsp-ui is optional. It provides inline overlays over
;; the symbol at point and enables code fixes at point. If you find it
;; to flashy and prefer not activating it just remove :config
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode).
;;
;; The config shown above already disables the documentation normally
;; shown inline by lsp-ui. This is too much for my taste as it often
;; covers up source code. If you want to also deactivate the
;; documentation shown in the minibuffer you can add (setq
;; lsp-eldoc-hook nil). To do less when your cursor moves consider
;; (setq lsp-signature-auto-activate nil) and (setq
;; lsp-enable-symbol-highlighting nil).
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(provide 'config-lsp)
;;; config-lsp ends here
