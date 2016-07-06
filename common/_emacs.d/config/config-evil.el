;;; config-evil --- Setup evil mode.

;;; Commentary:
;; Evil Mode. A/K/A Make Emacs Usable.

;;; Code:
;; set evil-mode by default, so emacs is actually usable as a text editor
(require 'evil)

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
;;(evil-leader/set-leader ",")
;; It requires remapping next selection, but ; is also nice.
;;(evil-leader/set-leader ";")
;; Let's give SPC a chance.
;;(evil-leader/set-leader "SPC")
(evil-leader/set-leader ",")

;; But why not use all three? TODO: Write evil-multileader
(evil-leader/set-key
  "," 'ibuffer
  ;; b is the prefix key for buffer operations. I'm not completely happy with
  ;; putting dired in with the buffer ops, but not sure where a better place
  ;; for it is
  "bc" 'clone-indirect-buffer-other-window
  "bf" 'diff-buffer-with-file
  "bk" 'kill-buffer
  "bd" 'narrow-to-defun
  "bn" 'narrow-to-region
  "bp" 'narrow-to-page
  "br" 'revert-buffer
  "bw" 'widen
  ;; "de" 'debug-on-error
  ;; "dE" 'debug-on-entry
  "df" 'describe-function
  "dk" 'describe-key
  "dv" 'describe-variable
  "fb" 'flycheck-buffer ; http://www.flycheck.org/
  "ff" 'flycheck-buffer
  "fl" 'flycheck-list-errors
  "fn" 'flycheck-next-error
  "fN" 'flycheck-previous-error
  "g" 'magit-status
  "nm" 'linum-mode
  "nn" 'linum-relative-toggle
  "pi" 'package-install
  "pl" 'package-list-packages
  "po" 'pianobar
  "pn" 'pianobar-next-song
  "pp" 'pianobar-play-or-pause
  "sb" 'eval-buffer
  "sl" 'eval-last-sexp
  "ss" 'eval-defun ;because eval-defun is *totally* a synonym for 'eval-this-sexp... dammit emacs
  ; Not sure this next one is a good idea - \z works to enter emacs mode, but not to leave it.
  "wf" 'toggle-frame-fullscreen
  "wn" 'make-frame
  "z" 'evil-emacs-state
  "[" 'keyboard-quit
  ;; (l)aunch application modes like dired, proced, ansi-term, et al
  "ld" 'dired
  "lp" 'proced)
  ;; "D" 'dired)

;; Org-mode specific keybindings.
;; http://stackoverflow.com/questions/25463369/mode-specific-or-buffer-local-text-objects-in-evil
;; Should these get wrapped in an eval-after-load?
(evil-leader/set-key-for-mode 'org-mode
  "a" 'org-agenda
  "cc" 'org-ctrl-c-ctrl-c
  "c'" 'org-edit-special
  "h" 'org-metaleft
  "k" 'org-metaup
  "j" 'org-metadown
  "l" 'org-metaright
  "t" 'org-todo)

;; This only works for major modes. Investigate how to do minor modes.
;; (evil-leader/set-key-for-mode 'org-src-mode
;;   "wq" 'org-edit-src-exit
;;   "q" 'org-edit-src-abort)

(add-hook 'org-mode-hook
          (lambda ()
            (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
            (evil-define-key 'normal org-mode-map (kbd "C-\\") 'org-insert-heading)
            (evil-define-key 'insert org-mode-map (kbd "C-\\") 'org-insert-heading)
            (auto-fill-mode)))

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

(set-evil-initial-mode 'normal
                       'ibuffer-mode
                       'package-menu-mode
                       'completion-list-mode)

(provide 'config-evil)
;;; config-evil.el ends here
