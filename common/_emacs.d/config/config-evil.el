;;; config-evil --- Setup evil mode.

;;; Commentary:
;; Evil Mode. A/K/A Make Emacs Usable.

;;; Code:

;; We're going to bind C-w for window management later, but to do so we need to
;; unbind it here first. Before unsetting, it's bound to 'kill-region, but we're
;; about have vim bindings for that.
(global-unset-key (kbd "C-w"))

;; Ensure evil and related packages are present.
(require 'package-tools)
(ensure-package-installed
 'evil ; A/K/A make emacs usable.
 'evil-leader
 'evil-indent-plus ; Indentation text objects: ii/ai, iI/aI, iJ/aJ
 'evil-goggles  ; Highlight yanking and other niceties
 ;;'evil-anzu
 ;;'evil-magit ;some time I should try getting this to work again
 ;;'evil-lisp-state
 ;;'evil-cleverparens ; Another lisp mode.
 ;;'evil-commentary ; Another commenter.
 ;;'evil-args
 ;;'evil-escape
 ;;'evil-god-state ; What's god-mode?
 ;;'evil-iedit ; What's iedit?
 ;;'evil-mc ; Multiple cursors?
 ;;'evil-nerd-commenter
 ;;'evil-org ; This has been problematic.
 ;;'evil-visualstar
 )

;; set evil-mode by default, so emacs is actually usable as a text editor
(require 'evil)
(require 'evil-leader)
(require 'evil-goggles)

;; First, set up C-d as a prefix key. To do that, we need a new key map.
; (define-prefix-command 'gbe-cmd-b-map)
; (global-unset-key (kbd "C-b"))
; (define-key evil-motion-state-map (kbd "C-b") nil)
; (global-set-key (kbd "C-b") 'gbe-cmd-b-map)
; (define-key gbe-cmd-b-map (kbd "b") 'evil-goto-line)
;; --> To define a function over a key mapped by evil, first set it to nil in
;; (in the motion state map, since those are usually the ones causing a
;; problem) and then set it to the desired function.

;; Note that we're shadowing evil-scroll-down here. It's also worth noting that
;; this is binding isn't, as far as I can tell, easily shadowable.
(evil-set-toggle-key "C-d")

;; Set up a indention textobject.
;;
;; The default keybinds are:
;;   * ii: A block of text with the same or higher indentation.
;;   * ai: The same as ii, plus whitespace.
;;   * iI: A block of text with the same or higher indentation, including the
;;         first line above with less indentation.
;;   * aI: The same as iI, plus whitespace.
;;   * iJ: A block of text with the same or higher indentation, including the
;;         first line above and below with less indentation.
;;   * aJ: The same as iJ, plus whitespace.
;;
;; See https://github.com/TheBB/evil-indent-plus for full details
(require 'evil-indent-plus)
(evil-indent-plus-default-bindings)

;; supposedly - http://nathantypanski.com/blog/2014-08-03-a-vim-like-emacs-config.html - evil
;; mode needs to start after everything else for things to work well

;; More messing around with evil keybinds:
;; https://www.reddit.com/r/evilmode/comments/323xh1/is_it_possible_to_use_vi_keybindings_everywhere/

;; Also, evil-magit has... issues. References:
;; https://github.com/justbur/evil-magit/issues/1
;; https://github.com/justbur/evil-magit/issues

;; I still want to figure out how to break up this one massive config into many,
;; but for now, I give up.
;; (require 'my-evil-config)

(global-evil-leader-mode) ; per the evil-leader readme, this should be enabled first, but might not actually have an effect
(evil-mode t)

;; Setup leader key stuff.
;; \ is the default, but "," is another common choice
(evil-leader/set-leader ",")

;; (defun comment-line-or-region ()
;;   "No."
;;   (interactive)
;;   (message "nil")
;;   nil)
;; (defun comment-line-or-region (arg1 arg2)
;;   "Stuff. ARG1 and ARG2 ignored."
;;   (interactive)
;;   (comment-line))
;; (commandp 'comment-line-or-region)
;; (comment-line-or-region)
;; (debug-on-entry 'comment-line-or-region)
;; (cancel-debug-on-entry 'comment-line-or-region)

(defun toggle-magit-blame ()
  "There isn't a single function to turn magit-blame on and off, so make one.
This doesn't actually work yet because of how blame-mode is implemented."
  (interactive)
  (if (member 'magit-blame-mode minor-mode-list)
      (message "t")
    (message "nil")))

(evil-leader/set-key
  "," 'ibuffer

  ;; Code folding. Also, comment-dwim has never once done what I mean
  "+" 'hs-show-block
  "-" 'hs-hide-block
  "[" 'fold-this
  "]" 'fold-this-unfold-at-point
  "aa" 'fold-this
  "<+" 'hs-show-all
  "<-" 'hs-hide-all
  "/" 'comment-dwim

  ;; b is the prefix key for buffer operations
  "ba" 'find-file ;I type ,bs for :e enough I want an escape hatch
  "bb" 'blacken-buffer  ; keybind, since vtrack means black-on-save is not viable
  "bc" 'clone-indirect-buffer-other-window
  "bd" 'diff-buffer-with-file
  "bf" 'json-pretty-print-buffer
  "bF" 'json-pretty-print-buffer-ordered
  ;"bfj" 'diff-buffer-with-file
  "bg" 'magit-blame-mode
  "bh" 'fci-mode ;(from buffer-highlight)
  "bk" 'kill-this-buffer ; It's _always_ the current buffer I want to kill. Thanks @brandon-rhodes for this one.
  "bn" 'narrow-to-defun ; It's (almost) always the definition I want to narrow to.
  "bN" 'narrow-to-region
  "bo" 'delete-other-windows
  "bp" 'narrow-to-page
  "br" 'revert-buffer
  ;"bs" 'switch-to-buffer
  "bs" 'helm-buffers-list
  "bS" 'gbe/change-directory-leaf
  "bw" 'widen

  ;; Searching (via ag/hound mostly) and describing
  ;; ag.el exports:
  ;;       ag           ag-project
  ;;       ag-files     ag-project-files
  ;;       ag-regexp    ag-project-regexp
  ;; We also put 'hound in this space. See https://github.com/ryoung786/hound.el
  ;; for configuration of hound.
  "da" 'ag
  "db" 'describe-bindings
  "dc" 'describe-char ; Mostly a novelty, but good to know. Again, from @brandon-rhodes.
  "de" 'gbe/describe-package-at-point
  "df" 'describe-function
  "dg" 'ag-files
  "dh" 'help
  "dk" 'describe-key
  "dn" 'hound
  "dp" 'ag-project
  "dP" 'gbe/describe-package-at-point
  "dr" 'ag-regexp
  "du" 'describe-face
  "dv" 'describe-variable
  ;;"dw" 'apropos ; Doesn't autofill object at point sadly
  ;;"dW" 'apropos-command
  ;;"dz" 'info-apropos

  ;; "ee" 'find-file-at-point

  ;; Flycheck and frame
  ;; "fb" 'flycheck-buffer ; http://www.flycheck.org/
  ;; "ff" 'flycheck-buffer
  ;; "fl" 'flycheck-list-errors
  ;; "fn" 'flycheck-next-error
  ;; "fN" 'flycheck-previous-error
  "fn" 'display-line-numbers-mode ; This mode can also do relative by setting display-line-numbers to 'relative
  "ft" 'transpose-frame
  "gb" 'magit-blame-start-or-quit
  ;;"gc" 'gcn
  "gf" 'find-file-at-point
  "gg" 'magit-status
  "gi" 'magit-init
  ;; TODO - do something real in the keybind map
  "ga" 'smerge-keep-all
  "gc" 'smerge-keep-current
  "gu" 'smerge-keep-upper
  "gd" 'smerge-keep-lower
  "gm" 'smerge-keep-mine
  "gt" 'smerge-keep-other ; "keep the one where my cursor isn't"
  "gn" 'smerge-next ; This should probably variously do smerge-next, magit-blame-next, etc
  "gp" 'smerge-prev
  "ii" 'insert-char
  ;; (l)aunch application modes like dired, proced, ansi-term, et al
  "ld" 'dired
  "lp" 'proced
  ;; "nm" 'linum-mode
  ;; "nn" 'linum-relative-toggle

  "o" 'occur

  ;; Packages and pianobar (not that I've been using pianobar much)
  "pi" 'package-install
  "pl" 'paradox-list-packages
  ;; "pl" 'package-list-packages
  "po" 'pianobar
  "pn" 'pianobar-next-song
  "pp" 'pianobar-play-or-pause
  "sb" 'eval-buffer
  "rc" 'comment-region
  "rt" 'indent-region
  "ru" 'uncomment-region
  "sd" 'transpose-sexps
  "sf" 'browse-url-of-file
  "sl" 'eval-last-sexp
  "so" 'browse-url
  "ss" 'eval-defun ;because eval-defun is *totally* a synonym for 'eval-this-sexp... dammit emacs
  "tf" 'transpose-frame
  "tt" 'my-run-pytest-from-buffer-name
  "ta" 'pytest-all

  "u" 'universal-argument
  "v" 'find-name-dired

  ;; Window operations
  "wf" 'toggle-frame-fullscreen
  "wh" 'other-frame
  "wk" 'delete-window
  "wl" 'reverse-other-frame
  "wn" 'make-frame
  )
  ;; "D" 'dired)

;; Add a binding to insert a newline without leaving normal state.
;; First we need an insert-newline function, that will dance us in and out of
;; the right modes.
(defun insert-newline ()
   "Insert a newline and return to appropriate evil state."
   (interactive)
   (let ((current-state evil-state))
     (evil-open-below 0)
     (unless (eq current-state 'insert)
       (evil-change-state current-state))))
(evil-global-set-key 'normal (kbd "g o") 'insert-newline)

(setq evil-leader/in-all-states t)

;; Try to kill custom buffer maps.
;;(setq evil-overriding-maps nil)
;;(setq evil-intercept-maps nil)

; Bad emacs! Escape is not a modifier key.
; minibuffer-keyboard-quit was a funtion... try just doing abort-recursive-edit directly?
; (define-key evil-normal-state-map [escape] 'keyboard-quit)
; (define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

;; Initial mode setting.
;;(evil-set-initial-state 'ibuffer-mode 'normal)

(defun set-evil-initial-mode (start-state &rest mode-list)
  "START-STATE MODE-LIST."
  ;;"Make each mode in the MODE-LIST start in the given START-STATE."
  (mapcar
   (lambda (mode) (evil-set-initial-state mode start-state))
   mode-list))

(set-evil-initial-mode
 'normal ; Start the following modes in 'normal state.
 'ibuffer-mode
 'completion-list-mode
 )
(add-hook 'git-commit-mode-hook 'evil-insert-state)

;; Mode registration should be a mode config thing, not an evil config thing.
;; That said, emacs has some built-in modes that don't make much sense
;; configuring anywhere else.
(add-to-list 'evil-emacs-state-modes 'messages-buffer-mode)
(add-to-list 'evil-emacs-state-modes 'image-mode) ;
;; We also need to set this for package-menu-mode here, since config-package is
;; loaded before evil mode as it's needed when bootstrapping.
(add-to-list 'evil-emacs-state-modes 'package-menu-mode)

(defun gbe/set-initial-state-for-modes (start-state &rest mode-list)
  "Make each mode in the MODE-LIST start in the given START-STATE."
  (mapcar
   (lambda (mode) (evil-set-initial-state mode start-state))
   mode-list))

;; Keybinds for emacs state
(define-key evil-emacs-state-map (kbd "C-;") 'evil-execute-in-normal-state)

;; Keybinds for emacs state
;(define-key evil-motion-state-map (kbd "n") 'compilation-next-error)
;(define-key evil-motion-state-map (kbd "p") 'compilation-previous-error)
(define-key evil-motion-state-map (kbd "n") 'evil-search-next)
(define-key evil-motion-state-map (kbd "p") 'evil-search-previous)

(gbe/set-initial-state-for-modes
 'motion
 'lsp-ui-imenu-mode
 'xref--xref-buffer-mode)

(gbe/set-initial-state-for-modes
 'emacs
 'eshell-mode
 'ibuffer-mode
 'messages-buffer-mode
 'term-mode)

(provide 'config-evil)
;;; config-evil.el ends here
