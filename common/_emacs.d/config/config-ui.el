;;; config-ui --- UI settings.

;;; Commentary:
;; First things first, make Emacs look less... well, emacs-y.
;; So, first we do a bunch of theme stuff, and then other appearance stuff.
;; Basically, if it's just a UI tweak or setting, it should live here.
;; Regarding themes, if you ever want to waste an afternoon, checkout
;;     http://emacsthemes.com/index/1.html

;;; Code:

;;;; Themes:
;; Ample's pretty darned nice.
(load-theme 'ample t t)
(enable-theme 'ample)
;; (load-theme 'ample-flat t t)
;; (load-theme 'ample-light t t)


;; Zenburn for emacs: https://github.com/bbatsov/zenburn-emacs
;; (load-theme 'zenburn t)
;; (enable-theme 'zenburn)

;; (load-theme 'warm-night)

;; Unofficial solarized port: https://github.com/bbatsov/solarized-emacs
;; There's also an official port at https://github.com/sellout/emacs-color-theme-solarized
;; with ties to the orginal project.
;; (load-theme 'solarized-dark)
;; (load-theme 'solarized-light)

;; Monokai by https://github.com/oneKelvinSmith/monokai-emacs
;; (load-theme 'monokai t)

;; Org-mode theme. Now figure out how to swap themes on mode-switch.
;; Repo is https://github.com/fniessen/emacs-leuven-theme
;; (load-theme 'leuven t)

;; One version of darcula is at https://github.com/fommil/darcula-theme-emacs
;; (require 'darcula-theme)
;; It's not clear to me if this is the package available on melpa, but I don't think it
;; is. To use that one, instead run
;; (load-theme 'darcula)

;;;; Font stuff:
;; The set-frame-font doesn't play nicely with 'make-frame, but defining a
;; default font does.
;; Setting Consolas-14 is more like 16pt, but it's a good size
;; TODO: This depends on OS & Environment.
(setq default-frame-alist '((font . "Consolas-14")))


;;;; General UI Tweaks: ;;
;; ain't no reason for that blasted splash screen
(setq inhibit-splash-screen t)

;; get rid of that awful tool bar... the menu bar can be similarly disabled but isn't nearly so bad
(tool-bar-mode -1)

;; Start off in linum-relative and column-number modes.
(linum-relative-mode t)

;; Don't do that freaking line wrap thing.
(setq-default truncate-lines t)

;; Scrollbars are ugly son. Supposedly this errors out on terminal mode, whence
;; the 'when as a guard.
(when (display-graphic-p) (set-scroll-bar-mode nil))

;; Global prettification. Because lambda is λ dammit.
(global-prettify-symbols-mode t)

;; At some point, I should get code folding to work.
;; Resources:
;; http://stackoverflow.com/questions/2399612/why-is-there-no-code-folding-in-emacs
;; http://stackoverflow.com/questions/15307113/emacs-cedet-semantic-tag-folding
;; http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
;; https://github.com/jorgenschaefer/elpy/issues/240
;; http://www.emacswiki.org/emacs/HideShow (hs-minor-mode)


(provide 'config-ui)
;;; config-ui.el ends here
