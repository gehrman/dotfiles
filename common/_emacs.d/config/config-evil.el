;;; config-evil --- Setup evil mode.

;;; Commentary:
;; Evil Mode. A/K/A Make Emacs Usable.

;;; Code:

;; Ensure evil and related packages are present.
(require 'package-tools)
(ensure-package-installed
 'evil ; A/K/A make emacs usable.
 'evil-leader
 'evil-indent-plus ; Indentation text objects.
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

;; Before we load continue loading evil/emacs, we want to set the toggle key to
;; ensure everything is initialized correctly.
(setq evil-toggle-key "C-d") ; Note that we're shadowing evil-scroll-down here.

;; set evil-mode by default, so emacs is actually usable as a text editor
(require 'evil)
(require 'evil-leader)

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

(defun beautify-json-buffer ()
  "Reformat json buffer."
  (interactive)
  (shell-command)
  (shell-command-on-region)
  )

;; But why not use all three? TODO: Write evil-multileader
(evil-leader/set-key
  "," 'ibuffer
  "+" 'hs-show-block
  "-" 'hs-hide-block
  "<+" 'hs-show-all
  "<-" 'hs-hide-all
  "/" 'comment-dwim
  ;; b is the prefix key for buffer operations. I'm not completely happy with
  ;; putting dired in with the buffer ops, but not sure where a better place
  ;; for it is
  "ba" 'find-file ;I type ,bs for :e enough I want an escape hatch
  "bc" 'clone-indirect-buffer-other-window
  "bd" 'diff-buffer-with-file
  ;"bfj" 'diff-buffer-with-file
  "bg" 'magit-blame-mode
  "bh" 'fci-mode ;(from buffer-highlight)
  "bk" 'kill-this-buffer ; It's _always_ the current buffer I want to kill. Thanks @brandon-rhodes for this one.
  "bn" 'narrow-to-defun ; It's (almost) always the definition I want to narrow to.
  "bN" 'narrow-to-region
  "bo" 'delete-other-windows
  "bp" 'narrow-to-page
  "br" 'revert-buffer
  "bs" 'switch-to-buffer
  "bs" 'switch-to-buffer
  "bw" 'widen
  ;; "de" 'debug-on-error
  ;; "dE" 'debug-on-entry

  ;; ag.el exports:
  ;;       ag           ag-project
  ;;       ag-files     ag-project-files
  ;;       ag-regexp    ag-project-regexp
  ;; We also put 'hound in this space. See https://github.com/ryoung786/hound.el
  ;; for configuration of hound.
  "da" 'ag
  "dc" 'describe-char ; Mostly a novelty, but good to know. Again, from @brandon-rhodes.
  "df" 'describe-function
  "dg" 'ag-files
  "dh" 'help
  "dk" 'describe-key
  "dn" 'hound
  "dp" 'ag-project
  "dP" 'describe-package-at-point ; Finish getthing this perfect
  "dr" 'ag-regexp
  "du" 'describe-face
  "dv" 'describe-variable
  ;;"dw" 'apropos ; Doesn't autofill object at point sadly
  "ee" 'find-file-at-point
  "fb" 'flycheck-buffer ; http://www.flycheck.org/
  "ff" 'flycheck-buffer
  "fl" 'flycheck-list-errors
  "fn" 'flycheck-next-error
  "fN" 'flycheck-previous-error
  "gb" 'magit-blame ; This should probably cycle the blame to keep gk free.
  "gf" 'find-file-at-point
  "gg" 'magit-status
  "gi" 'magit-init
  "gk" 'magit-blame-quit
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
  "nm" 'linum-mode
  "nn" 'linum-relative-toggle
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
  ; Not sure this next one is a good idea - \z works to enter emacs mode, but not to leave it.
  "wf" 'toggle-frame-fullscreen
  "wk" 'delete-window
  "wn" 'make-frame
  "z" 'evil-emacs-state
  "[" 'keyboard-quit
  )
  ;; "D" 'dired)

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

;; This, with some tweaks, should get C-w hjkl working in Emacs mode
;; (define-prefix-command 'evil-window-map)
;; (define-key evil-window-map "b" 'evil-window-bottom-right)
;; (define-key evil-window-map "c" 'evil-window-delete)
;; ...
;; (define-key evil-motion-state-map "\C-w" 'evil-window-map)

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

;; (set-evil-initial-mode
;;  'emacs                   ; Start the following modes in 'emacs state.
;;  '(ansi-term
;;    package-menu-mode
;;    messages-buffer-mode
;;    eshell
;;    )
;;  )

;; Mode registration should be a mode config thing, not an evil config thing.
(add-to-list 'evil-emacs-state-modes 'magit-blame-mode)
(add-to-list 'evil-emacs-state-modes 'messages-buffer-mode)
;;(add-to-list 'evil-emacs-state-modes 'docker-image-mode) ; moved to docker config
;;(set-evil-initial-mode 'ansi-term 'emacs)

(provide 'config-evil)
;;; config-evil.el ends here
