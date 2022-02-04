;;; Package -- Summary

;;; Commentary:
;; Configure ML family language handling - Haskell, Elm, Idris, and other ML languages

;;; Code:

;; ML Language family
(require 'package-tools)

(ensure-package-installed
 ;;'edts
 'elm-mode
 'erlang
 'haskell-mode
 'idris-mode
 'flycheck-elm
 ;;'elm-yasnippets
 ;;'flycheck-haskell
 ;;'dante (fork of intero)
 ;;'ghc
 ;;'haskell-emacs -- emacs extensions in haskell?!
 ;;'haskell-snippets
 ;;'hasky-extensions
 ;;'hi2/hident/hyai
 ;;'shm
 )

;; Elm Config

(require 'elm-mode)

;; This requires `elm-format` on $PATH, which may be installed
;; with `npm -g elm-format`. Note that it's also on homebrew
;; but brew installs versioned commands, not a generic one.
(setq elm-format-on-save t)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

;; Haskell Config
;; All of this is lifted directly from Patrick for now.

(require 'haskell-mode)
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
