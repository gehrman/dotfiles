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
;;(load-theme 'ample t t)
;;(enable-theme 'ample)
;; (load-theme 'ample-flat t t)
;; (load-theme 'ample-light t t)

;; Reykjavik sounds fun and looks nice.
(load-theme 'reykjavik t t)
(enable-theme 'reykjavik)


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
;; TODO: This depends on OS & Environment.
;; So rather than defining the default by pushing on the frame alist, the new standard approach
;; seems to be using 'set-face-attribute. Definitely more readable, at least.
;; (set-face-attribute 'default nil :family "Monaco" :height 150)
;; Monaco is a nice default fallback, since Consolas seems to have disappeared, but Inconsolata
;; is just gorgeous. Note that the InconsolataGo version uses a straight ".
;(set-face-attribute 'default nil :family "InconsolataGo" :height 180)
;(set-face-attribute 'default nil :family "InconsolataGo" :height 220)
;(set-face-attribute 'default nil :family "InconsolataGo" :height 240)
;(set-face-attribute 'default nil :family "Menlo" :height 180)
;; Some other options
;(set-face-attribute 'default nil :family "Fira Mono" :height 200) ; Vaguely "type-writery"
;(set-face-attribute 'default nil :family "Envy Code R" :height 200 :weight 'normal)
;(set-face-attribute 'default nil :family "Hermit" :height 160 :weight 'normal) ; Kinda sci-fi modernist
;(set-face-attribute 'default nil :family "Fantasque Sans Mono" :height 200 :weight 'normal) ; Comic-booky
;(set-face-attribute 'default nil :family "NovaMono" :height 180) ; An interesting one

;; ilIega1o = 0O == -> --> ############ # && && || || <> << >> >>= <<= /= =/= != ~=!=
(set-face-attribute 'default nil :family "Fira Code" :height 200) ; Vaguely "type-writery"
;(set-face-attribute 'default nil :family "Fira Code" :height 240) ; Vaguely "type-writery"
;(set-face-attribute 'default nil :family "Monoid" :height 180) ;
;(set-face-attribute 'default nil :family "Monoid HalfTight" :height 180) ;
;(set-face-attribute 'default nil :family "Hasklig" :height 200) ;
;(set-face-attribute 'default nil :family "Iosevka" :height 200) ;
;(set-face-attribute 'default nil :family "Iosevka Term" :height 200) ;
;(set-face-attribute 'default nil :family "Iosevka Slab" :height 200) ;
;(set-face-attribute 'default nil :family "IosevkaCC" :height 200) ;
;; Proportional:
;(set-face-attribute 'default nil :family "Tsukushi B Round Gothic" :height 200) ; very sleek I guess


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

;; Window manipulations
(require 'transpose-frame)

;; Again, usability content. Need to make this global.
(require 'visible-mark)
;(visible-mark-mode)

;; Line length stuff.
(require 'fill-column-indicator)
(setq fci-rule-column 80)

;; RBT Interactions
(require 'rbt)

;;; Ido-mode:
;; So ido-mode presents choices while doing things like switch buffers
;; by putting them in the the mini-buffer. As you type, options narrow
;; down to match the text you've typed.
(ido-mode t)

;; Automatch delimites
;(show-paren-mode 't)
;(setq show-paren-style "expression")

;; At some point, I should get code folding to work.
;; Resources:
;; http://stackoverflow.com/questions/2399612/why-is-there-no-code-folding-in-emacs
;; http://stackoverflow.com/questions/15307113/emacs-cedet-semantic-tag-folding
;; http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
;; https://github.com/jorgenschaefer/elpy/issues/240
;; http://www.emacswiki.org/emacs/HideShow (hs-minor-mode)

;; Global, non-evil keybinds. (When does it come time to spin this all off into
;; its own file?)
(global-set-key (kbd "C-c C-z") 'suspend-frame)
(global-set-key (kbd "s-q") 'delete-frame)
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

;; Mode-specific non-evil binds.
(eval-after-load 'ibuffer
  '(progn
     (define-key ibuffer-mode-map (kbd "N") 'ibuffer-forward-filter-group)
     (define-key ibuffer-mode-map (kbd "P") 'ibuffer-backward-filter-group)
     (define-key ibuffer-mode-map (kbd "TAB") 'ibuffer-backward-filter-group)))


(provide 'config-ui)
;;; config-ui.el ends here
