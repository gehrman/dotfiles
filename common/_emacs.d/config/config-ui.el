;;; config-ui --- UI settings.

;;; Commentary:
;; First things first, make Emacs look less... well, emacs-y.
;; If you ever want to waste an afternoon, checkout http://emacsthemes.com/index/1.html

;;; Code:
(require 'package-tools)

;; Themes
(ensure-package-installed
 'ample-theme
 'zenburn-theme
 'monokai-theme
 'darcula-theme
 'reykjavik-theme
 )

;; Non-theme UI
(ensure-package-installed
 ; 'all-the-icons
 ; 'all-the-icons-dired
 ; 'all-the-icons-ibuffer
 'bison-mode
 'fill-column-indicator
 'find-file-in-project
 'fold-this
 ;;'ido-ubiquitous
 'linum-relative
 'multi-term
 'projectile
 'rainbow-delimiters
 'rainbow-identifiers
 'smex
 'transpose-frame
 'visible-mark
 'visual-fill-column
 'which-key
 )

(require 'fold-this)
(require 'linum-relative)
(require 'smex)
(require 'projectile)
(require 'bison-mode)

;;;; Themes
(load-theme 'ample t)
;; (load-theme 'ample-flat t)

;;;; Font stuff
;; Test banner:
;; ilIega10oO == -> --> ############ # && && || || <> << >> =<< >>= <<= /= =/= != ~=!=
(set-face-attribute 'default nil :family "Comic Code Ligatures" :height 200)

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

;; Find File stuff
(require 'find-file-in-project)
(setq ffip-use-rust-fd t)

;; Global prettification. Because lambda is λ dammit.
(push '("forall" . ?∀) prettify-symbols-alist)
(push '("exists" . ?∃) prettify-symbols-alist)
(push '("not" . ?¬) prettify-symbols-alist)
(push '("implies" . ?⇒) prettify-symbols-alist)
(push '("iff" . ?⇔) prettify-symbols-alist)
(push '("lambda" . ?λ) prettify-symbols-alist)
(push '("def" . ?λ) prettify-symbols-alist)
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
      ;;(seq "." (not (any "." eol))) ;; Just omit hidden files that aren't . or ..
      (seq ".pyc" eol)
      (seq ".pyo" eol)
      )))
(setq
 dired-omit-extensions
 (append
  dired-latex-unclean-extensions
  dired-bibtex-unclean-extensions
  dired-texinfo-unclean-extensions
  ))
;; (setq
;;  dired-omit-files
;;  "^\\.?#\\|^\\.[^\\.$]+$"
;;  )
;; (setq
;;  dired-omit-files
;;  (append
;;   dired-latex-unclean-extensions
;;   dired-bibtex-unclean-extensions
;;   dired-texinfo-unclean-extensions
;;   ))
(setq dired-listing-switches "-alFh")
(setq dired-ls-F-marks-symlinks 't)
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode t)))

(defun my-evil-goto-top ()
  "Go to top."
  (interactive)
  (evil-goto-line 1))
(evil-leader/set-key-for-mode 'dired-mode
  "G" 'evil-goto-line
  "T" 'my-evil-goto-top
  )
(put 'dired-find-alternate-file 'disabled nil)

;; (setq dired-mode-hook nil)
;; (add-hook 'dired-mode-hook 'dired-extra-startup)
;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

;;; Ido-mode:
;; So ido-mode presents choices while doing things like switch buffers
;; by putting them in the the mini-buffer. As you type, options narrow
;; down to match the text you've typed.
;; See, e.g., https://www.masteringemacs.org/article/introduction-to-ido-mode
;; for customization options.
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-default-buffer-method "selected-window")
;; (with-no-warnings
;;   (require 'ido-ubiquitous))

;; Apparantly this one annoys a bunch of people, setting to nil disables.
;;(setq ido-use-filename-at-point nil)
;;(setq ido-use-filename-at-point 'guess)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; smex - fuzzy/filterable M-x
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
;; Do we make : 'smex too?
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
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(defun define-rainbow-delimiter-face-colors ()
  "Set rainbow-delimiter colors."
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
    (rainbow-delimiters-mismatched-face . "chartreuse")
    (rainbow-delimiters-unmatched-face . "firebrick2")
    )))
(add-hook 'rainbow-delimiters-mode-hook 'define-rainbow-delimiter-face-colors)
;; At some point, I should get code folding to work.
;; Resources:
;; http://stackoverflow.com/questions/2399612/why-is-there-no-code-folding-in-emacs
;; http://stackoverflow.com/questions/15307113/emacs-cedet-semantic-tag-folding
;; http://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
;; https://github.com/jorgenschaefer/elpy/issues/240
;; http://www.emacswiki.org/emacs/HideShow (hs-minor-mode)

;; Someday I'll learn what projectile does
(projectile-mode)

(defun kill-help-buffer ()
  "Does just that.

  Basically, this is here so that when help grabs an already existing pane
  we won't have to navigate to that pane to close help."
  (interactive)
  (kill-buffer "*Help*"))

(defun maximize-this-window ()
  "Short-cut to run Cw | and Cw _ - basically making one pane the maximum size."
  (interactive)
  (evil-window-set-width (frame-width))
  (evil-window-set-height (frame-height)))

;; Global, non-evil keybinds. (When does it come time to spin this all off into
;; its own file?)
(global-set-key (kbd "C-c C-z") 'suspend-frame)
(global-set-key (kbd "s-q") 'delete-frame)  ;; Why isn't this 'kill-emacs / unbound?
(global-set-key (kbd "C-<return>") 'start-or-kill-eshell) ;; In restclient-mode this is shadowed into sending the request. I hope.
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-<kp-enter>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-<kp-subtract>") 'hs-hide-block)
(global-set-key (kbd "C-s-<kp-subtract>") 'hs-hide-all)
(global-set-key (kbd "C-<kp-add>") 'hs-show-block)
(global-set-key (kbd "C-s-<kp-add>") 'hs-show-all)

;; Window management
(define-prefix-command 'gbe/window-management-map)
(global-set-key (kbd "C-w") 'gbe/window-management-map)
(define-key gbe/window-management-map "e" 'kill-help-buffer)
(define-key gbe/window-management-map "h" 'evil-window-left)
(define-key gbe/window-management-map "j" 'evil-window-down)
(define-key gbe/window-management-map "k" 'evil-window-up)
(define-key gbe/window-management-map "l" 'evil-window-right)
(define-key gbe/window-management-map "m" 'maximize-this-window)
(define-key gbe/window-management-map "o" 'delete-other-windows)
(define-key gbe/window-management-map "|" 'evil-window-set-width)
(define-key gbe/window-management-map "=" 'balance-windows)
(define-key gbe/window-management-map "," 'ibuffer)

;; B&T Keybinds for searching
;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;(global-set-key (kbd "C-M-s") 'isearch-forward)
;(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-f") 'isearch-forward-symbol-at-point)

;; Mode-specific non-evil binds.
(eval-after-load 'ibuffer
  '(progn
     (define-key ibuffer-mode-map (kbd "N") 'ibuffer-forward-filter-group)
     (define-key ibuffer-mode-map (kbd "P") 'ibuffer-backward-filter-group)
     (define-key ibuffer-mode-map (kbd "TAB") 'ibuffer-backward-filter-group)))

;; Settings from B&T for OS integration
(setq
 select-enable-clipboard t ; makes killing/yanking interact with the clipboard
 select-enable-primary t ; not clear what this does but it's recommended?
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
;;(require 'artist-mode)  ;;Give it a try.

;; Make _ count as a word element, not symbol
(modify-syntax-entry ?_ "w")

;; Occur Mode looks like a compile buffer, so I want n and p to be navigation.
;; TODO: defer this, use evil-hjkl / start occur in motion
(add-hook 'occur-mode-hook
          (lambda ()
            (local-set-key "j" 'evil-next-line)
            (local-set-key "n" 'evil-next-line)
            (local-set-key "k" 'evil-previous-line)
            (local-set-key "p" 'evil-previous-line)))
(defun gbe/occur-at-point ()
  "Run an occur-search populated with the word at point."
  (interactive)
  (let ((search-term (thing-at-point 'word t)))
    (occur search-term)))
(evil-leader/set-key
  "O" 'gbe/occur-at-point)

;; Which-key mode lists all key-binds, I think?
(which-key-setup-side-window-right)

;; This should be put in with the other utility functions I've written into a
;; .emacs/lisp/utils.el or somesuch, but that's going to have to happen as part
;; of the eternally forthcoming refactoring and cleanup
(defun gbe/change-directory-leaf ()
  "Change to an identical leaf in a separate branch of the directory hierachy.
As an example, if a/b/c/d/e is the current path, given b, B as inputs, change
the current directory to a/B/c/d/e."
  (interactive)
  (let ((cur-dir (cadr (split-string (pwd))))
        (cur-buffer (car (reverse (split-string (buffer-file-name) "/")))))
    (let ((cur-node (helm-comp-read "Current dir: " (reverse (split-string cur-dir "/")))))
      (let ((new-node (helm-comp-read
                           "New directory: "
                           (directory-files (replace-regexp-in-string (concat cur-node ".*") "" cur-dir)))))
        (find-file (concat (s-replace cur-node new-node cur-dir) cur-buffer))))))

(provide 'config-ui)
;;; config-ui.el ends here
