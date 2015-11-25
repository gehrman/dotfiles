;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A bunch of theme stuff, and then other appearance stuff. ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make emacs look less... well, emacs-y
;
; Zenburn for emacs: https://github.com/bbatsov/zenburn-emacs
; (load-theme 'zenburn t)
; (enable-theme 'zenburn)

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

;;;;;;;;;;;;;;;;;
;; Font stuff. ;;
;;;;;;;;;;;;;;;;;
; The set-frame-font doesn't play nicely with 'make-frame, but defining a
; default font does.
; Setting Consolas-14 is more like 16pt, but it's a good size
(setq default-frame-alist '((font . "Consolas-14")))


;;;;;;;;;;;;;;;;;;;;;;;;
;; General UI Tweaks. ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; ain't no reason for that blasted splash screen
(setq inhibit-splash-screen t)

;; get rid of that awful tool bar... the menu bar can be similarly disabled but isn't nearly so bad
(tool-bar-mode -1)

(provide 'config-ui)
