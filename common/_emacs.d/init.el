;; init --- Top level configuration.

;;; Commentary:
;; There's still some package config stuff that should be pushed down into
;; their own files, but it's mostly clean now. The custom variable stuff
;; is probably here to stay too.

;;; Code:

;; Setup the path for custom config files.
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; We need to exec 'config-package first because it deals with setting up the
;; paths for all the installed packages, e.g. evil.
(require 'config-package)

;; Evil makes things usable, so it goes first to defend against errors in other
;; configs knocking out the keybindings.
(require 'config-evil)

;; Next up, make it pretty.
(require 'config-ui)

;; Now we can load the other configs.
;; TODO: pull these from the directory and load them programmatically
(require 'config-codeblocks)
(require 'config-flycheck)
(require 'config-latex)
(require 'config-local)
(require 'config-magit)
(require 'config-misc)
(require 'config-powerline)
(require 'config-python)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Answer: Tramp, Magit, Org-Mode.               ;;
;; Question: Why muck through the crap of Emacs. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Also, true client server?! Just need to make it work in Windows.
;; http://wikemacs.org/wiki/Emacs_server
;; http://emacs-fu.blogspot.com/2009/03/windows-and-daemons.html

;; General windows stuff.
;; http://www.emacswiki.org/emacs/EmacsMsWindowsIntegration

;; Python stuff. Do eet like, yesterday.
;; http://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;; http://www.emacswiki.org/emacs/ProgrammingWithPythonModeDotEl
;; https://github.com/jorgenschaefer/elpy
;; http://www.enigmacurry.com/2008/05/09/emacs-as-a-powerful-python-ide/
;; https://github.com/python-rope/ropemacs
;; http://wenshanren.org/?p=351/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Anzu is fun - displays current & total matches by highlighting. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (anzu-mode +1)
(global-anzu-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Minimap mode: Because it turns out sublimity sucks. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(minimap-mode t)
;(minimap-create)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making Emacs not suck. A work-in-progress. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No emacs, windows encoding is never ok.
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
;; Ok, so that's a bit of suspenders-and-belting. But I *really* don't want to
;; deal with a whitespace divergence on files ever ever.

;; Basic editor functionality. Includes, but not limited to, not inserting tab literals.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; See http://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode,
;; but this one might be best handled by the python mode, I'll grudgingly admit.

;; Let's trim trailing whitespace while we're at it.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; because the system bell is a good idea, said no-one ever.
(setq ring-bell-function nil)

;; OS aware base directory.
(if (eq system-type 'windows-nt)
    (progn
      (cd "d:/dev/code")
      (setq exec-path
	    (quote
	     ("c:/Program Files (x86)/Google/Chrome/Application"
        "C:/WINDOWS/system32"
        "C:/WINDOWS"
        "C:/WINDOWS/System32/Wbem"
        "C:/Program Files (x86)/Microsoft SQL Server/110/Tools/Binn/ManagementStudio/"
        "C:/Program Files (x86)/Microsoft SQL Server/110/Tools/Binn/"
        "C:/Program Files/Microsoft SQL Server/110/Tools/Binn/"
        "C:/Program Files (x86)/Microsoft SQL Server/110/DTS/Binn/"
        "C:/Program Files/Microsoft SQL Server/110/DTS/Binn/"
        "C:/Program Files/TortoiseSVN/bin"
        "C:/WINDOWS/System32/WindowsPowerShell/v1.0/"
        "d:/bin/emacs/24.5/libexec/emacs/24.5/i686-pc-mingw32"
        "C:/Program Files/Git/bin"
        "C:/Program Files/Git/usr/bin"
        "C:/Program Files/Git/mingw64/bin"
        "d:/bin/python/python27/Scripts"
        "d:/bin"))))
  (cd "~/devel/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized Variables. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs set these based on stuff.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0ec59d997a305e938d9ec8f63263a8fc12e17990aafc36ff3aff9bc5c5a202f0" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" default)))
 '(minimap-window-location (quote right)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#111111" :foreground "#bdbdb3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Menlo")))))

(provide 'init)
;;; init.el ends here
