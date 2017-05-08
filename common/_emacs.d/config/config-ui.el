;;; config-ui --- UI settings.

;;; Commentary:
;; First things first, make Emacs look less... well, emacs-y.
;; If you ever want to waste an afternoon, checkout http://emacsthemes.com/index/1.html

;;; Code:

;;;; Themes
;; (load-theme 'ample t t)
;; (enable-theme 'ample)
;; (load-theme 'ample-flat t t)
;; (enable-theme 'ample-flat)
(load-theme 'reykjavik t t)
(enable-theme 'reykjavik)

;;;; Font stuff
;; Test banner:
;; ilIega1o = 0O == -> --> ############ # && && || || <> << >> >>= <<= /= =/= != ~=!=
(set-face-attribute 'default nil :family "FuraCode Nerd Font" :height 200)
;(set-face-attribute 'default nil :family "InconsolataGo" :height 180)
;(set-face-attribute 'default nil :family "Fira Code" :height 200)

;;;; General UI Tweaks: ;;
;; ain't no reason for that blasted splash screen
(setq inhibit-splash-screen t)

;; Set the buffer & path name as the window title, because we can.
(setq-default frame-title-format "%b (%f)")

;; get rid of that awful tool bar... the menu bar can be similarly disabled but isn't nearly so bad
(tool-bar-mode -1)

;; Uniquification
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

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
;; Also... what _does_ this one do?
(require 'visible-mark)
(visible-mark-mode)

(global-hl-line-mode t)

;; Dired usability tweaks
(require 'dired-x) ; Enable dired e(x)tras
(setq
 dired-omit-files
 (rx (or
      (seq bol (? ".") "#") ;; Don't show emacs autosaves
      (seq "~" eol) ;; Don't show emacs backup files
      (seq ".pyc" eol)
      (seq ".pyo" eol)
      )))
(setq
 dired-omit-files
 (append
  dired-latex-unclean-extensions
  dired-bibtex-unclean-extensions
  dired-texinfo-unclean-extensions
  ))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode t)))
(put 'dired-find-alternate-file 'disabled nil)


;;; Ido-mode:
;; So ido-mode presents choices while doing things like switch buffers
;; by putting them in the the mini-buffer. As you type, options narrow
;; down to match the text you've typed.
;; See, e.g., https://www.masteringemacs.org/article/introduction-to-ido-mode
;; for customization options.
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-ubiquitous-mode t)

;; Apparantly this one annoys a bunch of people, setting to nil disables.
;;(setq ido-use-filename-at-point nil)
;;(setq ido-use-filename-at-point 'guess)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; smex - fuzzy/filterable M-x
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
;; See http://stackoverflow.com/questions/25824493/smex-in-evil-mode
;; ...basically it's not clear smex+evil play nice.
;;(define-key evil-motion-state-map ":" 'smex)
;;(define-key evil-motion-state-map ";" 'evil-ex)

;; Leaving this commented since I've kinda gotten used to it.
;;(setq electric-indent-mode nil)

;; Highlight matching parenthesis and color pairs
;; This needs to be themed so that point is the highlight color, rather than the
;; matching paren. The highlighting is wrong though, and makes it look like point
;; is on the other paren. So just use rainbow-delimiters. Womp.
;;(show-paren-mode 't)
;;(set-face-background 'show-paren-match "grey")
;(setq show-paren-style 'expression)
;(setq show-paren-style 'mixed)
(require 'rainbow-delimiters)

(add-hook
 'prog-mode-hook
 'rainbow-delimiters-mode)

(add-hook
 'prog-mode-hook
 (lambda ()
   (mapc
    (lambda (face-color-pair) (set-face-foreground (car face-color-pair) (cdr face-color-pair)))
    '((rainbow-delimiters-depth-1-face . "grey")
      (rainbow-delimiters-depth-2-face . "forest green")
      (rainbow-delimiters-depth-3-face . "royal blue")
      (rainbow-delimiters-depth-4-face . "dark orange")
      (rainbow-delimiters-depth-5-face . "dark orchid")
      (rainbow-delimiters-depth-6-face . "salmon4" )
      (rainbow-delimiters-depth-7-face . "goldenrod")
      (rainbow-delimiters-depth-8-face . "slate gray")
      (rainbow-delimiters-depth-9-face . "spring green")
      (rainbow-delimiters-mismatched-face . "firebrick2")
      (rainbow-delimiters-unmatched-face . "chartreuse")
      ))))

;; At some point, I should get code folding to work.
;; Resources:
;; http://stackoverflow.com/questions/2399612/why-is-there-no-code-folding-in-emacs
;; http://stackoverflow.com/questions/15307113/emacs-cedet-semantic-tag-folding
;; http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
;; https://github.com/jorgenschaefer/elpy/issues/240
;; http://www.emacswiki.org/emacs/HideShow (hs-minor-mode)

;; Someday I'll learn what projectile does
(projectile-global-mode)

;; Global, non-evil keybinds. (When does it come time to spin this all off into
;; its own file?)
(global-set-key (kbd "C-c C-z") 'suspend-frame)
(global-set-key (kbd "s-q") 'delete-frame)
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

;; B&T Keybinds for searching
;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;(global-set-key (kbd "C-M-s") 'isearch-forward)
;(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Mode-specific non-evil binds.
(eval-after-load 'ibuffer
  '(progn
     (define-key ibuffer-mode-map (kbd "N") 'ibuffer-forward-filter-group)
     (define-key ibuffer-mode-map (kbd "P") 'ibuffer-backward-filter-group)
     (define-key ibuffer-mode-map (kbd "TAB") 'ibuffer-backward-filter-group)))

;; Settings from B&T for OS integration
(setq
 x-select-enable-clipboard t ; makes killing/yanking interact with the clipboard
 x-select-enable-primary t ; not clear what this does but it's recommended?
 mouse-yank-at-point t ; Mouse yank commands yank at point instead of at click.

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t
 )

;; Maybe some day I'll find out what hippie-expand's all about.
;; This is a Lisp-friendly hippie expand from B&T
;; (global-set-key (kbd "M-/") 'hippie-expand)
;; (setq hippie-expand-try-functions-list
;;       '(try-expand-dabbrev
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-complete-lisp-symbol-partially
;;         try-complete-lisp-symbol))

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;;(require 'thing-at-point)

(provide 'config-ui)
;;; config-ui.el ends here
