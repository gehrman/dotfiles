;;; Package -- Summary

;;; Commentary:
;; Configure ML family language handling - Haskell, Elm, Idris, and other ML languages

;;; Code:

;; Elm Config

;;(require 'elm-mode)

;; Haskell Config
;; All of this is lifted directly from Patrick for now.

(require 'haskell-mode)
(ensure-package-installed 'intero)

(add-hook 'haskell-mode-hook 'intero-mode)
;; (mapc #'(lambda (hook)
;;          (add-hook 'haskell-mode-hook hook))
;;       '(interactive-haskell-mode
;;         haskell-indentation-mode
;;         haskell-doc-mode))

;; (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
;; (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;; (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;; (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; ;; Hoogle
;; (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
;; (setq haskell-hoogle-command "hoogle")

(provide 'config-ml)
;;; config-ml.el ends here
