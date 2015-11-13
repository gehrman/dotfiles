; Setup path for custom config files.
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(setq mytest (expand-file-name "config" user-emacs-directory))
;(require 'includetest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Package: the package manager. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setup package management.
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(defun ensure-package-installed (&rest packages)
  "Ensure every package listed is installed. If a package is not installed, try to install it.
   Returns a list of installed packages, or nil if every package is skipped."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
	 nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	 (package-install package)
	 package)))
   packages))

; Make sure we have package data.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

; Activate installed packages.
(setq package-enable-at-startup nil)
(package-initialize)

; Packages to use.
(ensure-package-installed 'evil
			  'evil-leader
			  'magit
			  'sublimity
			  'anzu
			  'linum-relative
			  'powerline
			  'powerline-evil
			  'helm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Powerline, without the pain of the vim install. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'powerline)
(require 'powerline-evil)
(powerline-default-theme)


;;;;;;;;;;;;;;;
; Magit Stuff ;
;;;;;;;;;;;;;;;
; Shut magit up.
(setq magit-last-seen-setup-instructions "1.4.0")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Anzu is fun - displays current & total matches by highlighting. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (anzu-mode +1)
(global-anzu-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sublime-style minimap. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;
; Try uncommenting all of these, opening a reasonably large file, and maximizing
; emacs. Yeah, not good, and I don't want to try and fix it right now.
; (require 'sublimity)
; (require 'sublimity-scroll)
; (require 'sublimity-map)
; (require 'sublimity-attractive)
;
; ; Don't display scrollbars, because the minimap looks ugly with them displayed.
; (sublimity-attractive-hide-bars)
; (sublimity-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Making Emacs not suck. A work-in-progress. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; No emacs, windows encoding is never ok.
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
; Ok, so that's a bit of suspenders-and-belting. But I *really* don't want to
; deal with a whitespace divergence on files ever ever.

; ain't no reason for that blasted splash screen
(setq inhibit-splash-screen t)

; get rid of that awful tool bar... the menu bar can be similarly disabled but isn't nearly so bad
(tool-bar-mode -1)

; because the system bell is a good idea, said no-one ever.
(setq visible-bell t)
(setq ring-bell-function nil)

; Start off in linum-relative and column-number modes.
(linum-relative-mode t)

; Don't do that freaking line wrap thing.
(setq-default truncate-lines t)

; Emacs set these based on stuff.
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
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A bunch of theme stuff, and then other appearance stuff. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make emacs look less... well, emacs-y
;
; Zenburn for emacs: https://github.com/bbatsov/zenburn-emacs
; (load-theme 'zenburn t)
; (enable-theme 'zenburn)

; Start in /d/devel. I hope.
(cd "d:/devel/")

; If you ever want to waste an afternoon, checkout
; http://emacsthemes.com/index/1.html

; Ample's pretty darned nice.
(load-theme 'ample t t)
(enable-theme 'ample)
; (load-theme 'ample-flat t t)
; (load-theme 'ample-light t t)

; (load-theme 'warm-night)

; Unofficial solarized port: https://github.com/bbatsov/solarized-emacs
; (load-theme 'solarized-dark)
; (load-theme 'solarized-light)

; There's also an official port at https://github.com/sellout/emacs-color-theme-solarized
; with ties to the orginal project.

; Monokai by https://github.com/oneKelvinSmith/monokai-emacs
; (load-theme 'monokai t)

; Org-mode theme. Now figure out how to swap themes on mode-switch.
; Repo is https://github.com/fniessen/emacs-leuven-theme
; (load-theme 'leuven t)

; One version of darcula is at https://github.com/fommil/darcula-theme-emacs
; (require 'darcula-theme)
; It's not clear to me if this is the package available on melpa, but I don't think it
; is. To use that one, instead run
; (load-theme 'darcula)

; The set-frame-font doesn't play nicely with 'make-frame, but defining a
; default font does.
; Setting Consolas-14 is more like 16pt, but it's a good size
(setq default-frame-alist '((font . "Consolas-14")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Evil Mode. A/K/A Make Emacs Usable. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set evil-mode by default, so emacs is actually usable as a text editor
(require 'evil)

; supposedly - http://nathantypanski.com/blog/2014-08-03-a-vim-like-emacs-config.html - evil
; mode needs to start after everything else for things to work well

; I still want to figure out how to break up this one massive config into many,
; but for now, I give up.
; (require 'my-evil-config)

(global-evil-leader-mode) ; per the evil-leader readme, this should be enabled first, but might not actually have an effect
(evil-mode t)

; setup leader key stuff
; let's stick with \ for now, but , is another common choice
;(evil-leader/set-leader ",")
; It requires remapping next selection, but ; is also nice.
;(evil-leader/set-leader ";")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "g" 'magit-status
  "m" 'linum-mode
  "n" 'linum-relative-mode
  "s" 'eval-last-sexp
  "x" 'execute-extended-command
  "F" 'describe-function
  "K" 'describe-key
  "V" 'describe-variable
  "[" 'keyboard-quit)
(setq evil-leader/in-all-states 1)

; Try to kill custom buffer maps.
;(setq evil-overriding-maps nil)
;(setq evil-intercept-maps nil)

; Bad emacs! Escape is not a modifier key.
; minibuffer-keyboard-quit was a funtion... try just doing abort-recursive-edit directly?
; (define-key evil-normal-state-map [escape] 'keyboard-quit)
; (define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

; org-mode stuff
(evil-leader/set-key-for-mode 'org-mode
  "t" 'org-set-tags
  "a" 'org-agenda
  )
(add-hook 'org-mode-hook
	  (lambda ()
	    (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
	    (evil-define-key 'normal org-mode-map (kbd "C-\\") 'org-insert-heading)
	    (evil-define-key 'insert org-mode-map (kbd "C-\\") 'org-insert-heading)
	    (auto-fill-mode)))
