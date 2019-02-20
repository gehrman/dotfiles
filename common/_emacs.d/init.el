;; init --- Top level configuration.

;;; Commentary:
;; There's still some package config stuff that should be pushed down into
;; their own files, but it's mostly clean now. The custom variable stuff
;; is probably here to stay too.

;;; Code:
;; Setup the path for custom config files.

;; Let's try some performance tuning. This is blatantly stolen from Fanael's
;; init.el, where he claims that Emacs GC defaults are too conservative for
;; modern machines.
(setq gc-cons-threshold (* 4 1024 1024))
(setq gc-cons-percentage 0.3)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Setup the path for custom config files.
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq tramp-remote-path '("/usr/local/bin" "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/sbin" "/csnzoo/gehrman/.bin"))

(require 'package-tools)
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
(mapc
 #'require
 '(config-addons
   config-ansi-term
   config-company
   config-docker
   config-flycheck
   config-fonthandling
   config-latex
   config-lisp
   config-local
   config-magit
   config-markup
   config-misc
   config-ml
   config-orgmode
   config-modeline
   config-python
   config-outlook
   config-webdev
 ))


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

;; No need for ~ files when editing... or maybe it's the .# files
(setq create-lockfiles nil)

;; Always follow vc symlinks
(setq vc-follow-symlinks t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPG Stuff, not yet enough settings to move to a config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note that on OS X, in addition to all the packages pulled in by
;; `brew install gpg2`, we also need to `brew install pinentry-mac` to get
;; EPA/GPG support working (at least when using a unified GUI-terminal emacs
;; session. It may be possible to get things working terminal-only w/o
;; `pinentry-mac`. It may also be that this is another problem related to
;; (setenv/getenv PATH) stuff. See config-osx.
(setq epg-gpg-program "gpg2")
;(setq epa-file-select-keys nil)


;;;;;;;;;;;;;;
;; Ag Stuff ;;
;;;;;;;;;;;;;;
;; Open selection in results window. Let's see how this feels. For refactoring,
;; blowing away the search isn't nice, but otherwise it's the right UX.
(setq ag-reuse-window 't)
;; (setq ag-reuse-window nil)
;; Reuse the same *ag* buffer for all searchs. Again, let's see how it feels.
(setq ag-reuse-buffers 't)
;; Focus the search buffer. (So nice.)
(add-hook 'ag-search-finished-hook (lambda () (other-window 1)))

;;;;;;;;;;;;;;;;;
;; Hound stuff ;;
;;;;;;;;;;;;;;;;;
(setq hound-host "hound.csnzoo.com")
(setq hound-root-directory "~/Devel")

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
    ("2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "9b35c097a5025d5da1c97dba45fed027e4fb92faecbd2f89c2a79d2d80975181" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "e491d84f66d5d540e8459e9af173789d6b70e42a57b2a537823441b6921b39bd" "41c8c11f649ba2832347fe16fe85cf66dafe5213ff4d659182e25378f9cfc183" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "a0dc0c1805398db495ecda1994c744ad1a91a9455f2a17b59b716f72d3585dde" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "ab0950f92dc5e6b667276888cb0cdbc35fd1c16f667170a62c15bd3ed5ae5c5a" "0ec59d997a305e938d9ec8f63263a8fc12e17990aafc36ff3aff9bc5c5a202f0" "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" default)))
 '(minimap-window-location (quote right))
 '(package-selected-packages
   (quote
    (erlang blacken tao-theme tangotango-theme tango-plus-theme material-theme cyberpunk-theme alect-themes grandshell-theme noctilux-theme afternoon-theme flatland-theme afterglow color-theme-solarized solarized rainbow-identifiers persistent-scratch emacs-xkcd intero docean docker-compose-mode dockerfile-mode docker daemons exec-path-from-shell calfw virtualenvwrapper markdown-mode tagedit json-mode company-flx browse-kill-ring smbc smex company-jedi ein ibuffer-vc visual-fill-column rbt fill-column-indicator vagrant-tramp ibuffer-tramp ess reykjavik-theme rustfmt cargo ensime scala-mode visible-mark transpose-frame zenburn-theme warm-night-theme vertica vagrant tomatinho solarized-theme restclient-test restclient-helm puppet-mode pomodoro pianobar paredit ob-restclient ob-http monokai-theme magit linum-relative latex-pretty-symbols hound helm-ag flycheck evil-leader evil-indent-plus elm-mode darcula-theme company-restclient cdlatex anzu ample-theme ag)))
 '(paradox-github-token t)
 '(sentence-end-double-space nil)
 '(visual-line-fringe-indicators (quote (nil |))))

;; Emacs, stop with the custom-faces thing. Stahp.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(telephone-line-evil-normal ((t (:inherit telephone-line-evil :background "blue")))))

(put 'narrow-to-region 'disabled nil)

(send-notification-from-emacs "Done starting up.")

(provide 'init)
;;; init.el ends here
