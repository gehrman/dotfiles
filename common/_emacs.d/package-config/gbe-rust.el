;;; gbe-rust --- modern (use-package) configuration for rust and related work

;;; Commentary:
;; Much of this is taken directly from
;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/, but the
;; leader binds are actually mine

;;; Code:
(use-package rustic
  :ensure
  :config
  (setq rustic-format-on-save t)
  (setq rustic-format-trigger 'on-save)
  (setq rustic-format-on-save-method 'rustic-format-buffer)
  )

;; Rust-specific leader binds
(evil-leader/set-key-for-mode 'rustic-mode
  ;; M-. and M-, in vanilla, but ,, is ibuffer, so use / instead
  "." 'lsp-find-definition
  "/" 'xref-pop-marker-stack
  "jj" 'lsp-ui-imenu ;; j for lsp stuff I guess?
  "jr" 'lsp-rename
  "?" 'lsp-find-references
  "f." 'lsp-find-definition
  "f," 'xref-pop-marker-stack
;;   "<tab>" 'company-indent-or-complete-common
;;   "." 'rust-format-buffer
;;   "c" 'rust-compile
   "ss" 'cargo-process-build
   "sd" 'cargo-process-run
)

;; Auto-format on save
;(add-hook 'before-save-hook 'rust-format-buffer)

;; Not working, maybe because of LSP errors?
;; (defun gbe/maybe-cargo-fmt-buffer ()
;;   "Run rustic-cargo-fmt on save if the buffer is not narrowed.
;;
;;    This is useful because running black on a narrowed buffer will break
;;    the narrowing. The checking major mode thing is a hack, and really
;;    this all needs to be upstreamed to blacken."
;;   (interactive)
;;   (when (equal major-mode 'rustic-mode)
;;     (rustic-cargo-fmt)))
;; (add-hook 'before-save-hook 'gbe/maybe-cargo-fmt-buffer)

;; Package pest-mode is available.
;;      Status: Available from melpa-stable -- Install
;;     Archive: melpa-stable
;;     Version: 0.1.0
;;      Commit: 43447a2c70f98edd1139005e32f437d3f142442b
;;     Summary: Major mode for editing Pest files
;;    Requires: emacs-26.3
;;     Website: https://github.com/ksqsf/pest-mode
;;    Keywords: languages
;;  Maintainer: ksqsf <i@ksqsf.moe>
;;      Author: ksqsf <i@ksqsf.moe>
;; Other versions: 20221231.15 (melpa).
;;   This package provides GNU Emacs major modes for editing Pest
;; grammar files.  Currently, it supports syntax highlighting,
;; indentation, imenu integration.
;;   Syntax checking is available from flymake-pest or flycheck-pest.
;;   Also, you can use `pest-test-grammar' to open a new buffer, in
;; which you can experiment with your language defined by the
;; grammar.  In this new buffer, you can use `pest-analyze-input'
;; (default keybinding: C-c C-c) to analyze the input, which will
;; give you an analysis report of the structure.  Also, if
;; `eldoc-mode' is enabled, put the point anywhere under an
;; grammatical element, a path on the parse tree will be shown in
;; the minibuffer.
(use-package pest-mode
  :straight t
  :hook (pest-mode . eldoc-mode))

(provide 'gbe-rust)
;;; gbe-rust.el ends here
