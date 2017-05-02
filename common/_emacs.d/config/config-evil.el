;;; config-evil --- Setup evil mode.

;;; Commentary:
;; Evil Mode. A/K/A Make Emacs Usable.

;;; Code:
;; Before we load emacs, we want to set the toggle key to ensure everything
;; is initialized correctly.
(setq evil-toggle-key "C-d") ; Note that we're shadowing evil-scroll-down here.

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
(evil-leader/set-leader ",")

;; But why not use all three? TODO: Write evil-multileader
(evil-leader/set-key
  "," 'ibuffer
  ;; b is the prefix key for buffer operations. I'm not completely happy with
  ;; putting dired in with the buffer ops, but not sure where a better place
  ;; for it is
  "ba" 'find-file ;I type ,bs for :e enough I want an escape hatch
  "bc" 'clone-indirect-buffer-other-window
  "bf" 'diff-buffer-with-file
  "bh" 'fci-mode ;(from buffer-highlight)
  "bk" 'kill-buffer
  "bd" 'narrow-to-defun
  "bn" 'narrow-to-region
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
  "df" 'describe-function
  "dg" 'ag-files
  "dh" 'help
  "dk" 'describe-key
  "dn" 'hound
  "dp" 'ag-project
  "dr" 'ag-regexp
  "dv" 'describe-variable
  ;;"dw" 'apropos ; Doesn't autofill object at point sadly
  "fb" 'flycheck-buffer ; http://www.flycheck.org/
  "ff" 'flycheck-buffer
  "fl" 'flycheck-list-errors
  "fn" 'flycheck-next-error
  "fN" 'flycheck-previous-error
  "gf" 'find-file-at-point
  "gg" 'magit-status
  ;; TODO - do something real in the keybind map
  "gm" 'smerge-keep-mine
  "gn" 'smerge-next
  "gp" 'smerge-prev
  "gt" 'smerge-keep-other ; "keep theirs"
  "ii" 'insert-char
  "nm" 'linum-mode
  "nn" 'linum-relative-toggle
  "pi" 'package-install
  "pl" 'package-list-packages
  "po" 'pianobar
  "pn" 'pianobar-next-song
  "pp" 'pianobar-play-or-pause
  "sb" 'eval-buffer
  "sf" 'browse-url-of-file
  "sl" 'eval-last-sexp
  "so" 'browse-url
  "ss" 'eval-defun ;because eval-defun is *totally* a synonym for 'eval-this-sexp... dammit emacs
  "rc" 'comment-region
  "rt" 'indent-region
  "ru" 'uncomment-region
  ; Not sure this next one is a good idea - \z works to enter emacs mode, but not to leave it.
  "wf" 'toggle-frame-fullscreen
  "wn" 'make-frame
  "z" 'evil-emacs-state
  "[" 'keyboard-quit
  ;; (l)aunch application modes like dired, proced, ansi-term, et al
  "ld" 'dired
  "lp" 'proced
  )
  ;; "D" 'dired)

;; Org-mode specific keybindings.
;; http://stackoverflow.com/questions/25463369/mode-specific-or-buffer-local-text-objects-in-evil
;; Should these get wrapped in an eval-after-load?
(evil-leader/set-key-for-mode 'org-mode
  "\\" 'org-insert-heading
  "a" 'org-agenda
  "cc" 'org-ctrl-c-ctrl-c
  "c'" 'org-edit-special
  "h" 'org-metaleft
  "k" 'org-metaup
  "j" 'org-metadown
  "l" 'org-metaright
  "sj" 'org-babel-next-src-block
  "sk" 'org-babel-previous-src-block
  "ss" 'org-babel-execute-src-block
  "t" 'org-todo
  )

;; This only works for major modes. Investigate how to do minor modes.
;; (evil-leader/set-key-for-mode 'org-src-mode
;;   "wq" 'org-edit-src-exit
;;   "q" 'org-edit-src-abort)
;; I think the way to do this will be to add post-load advice around
;; the 'org-edit-special function to manipulate that mode's keybinds.
;; The exact way to grab the keybind map and add temporary org-src-mode
;; key binds isn't clear yet. For more details, and a partially relevant
;; example, see http://emacs.stackexchange.com/a/20397.

(add-hook 'org-mode-hook
          (lambda ()
            (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
            (evil-define-key 'normal org-mode-map (kbd "C-\\") 'org-insert-heading)
            (evil-define-key 'insert org-mode-map (kbd "C-\\") 'org-insert-heading)
            (auto-fill-mode)))

(add-hook 'magit-mode-hook
          (lambda ()
            (evil-define-key 'emacs magit-mode-map (kbd ", ,") 'ibuffer)
            (evil-define-key 'emacs magit-mode-map (kbd ", .") 'delete-other-windows)
            (evil-define-key 'emacs magit-mode-map (kbd ", w j") 'evil-window-left)
            (evil-define-key 'emacs magit-mode-map (kbd ", w l") 'evil-window-right)
          ))

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
(set-evil-initial-mode 'emacs
                       'ansi-term)

(provide 'config-evil)
;;; config-evil.el ends here
