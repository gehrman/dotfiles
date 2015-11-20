;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Mode. A/K/A Make Emacs Usable. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set evil-mode by default, so emacs is actually usable as a text editor
(require 'evil)

;; supposedly - http://nathantypanski.com/blog/2014-08-03-a-vim-like-emacs-config.html - evil
;; mode needs to start after everything else for things to work well

;; More messing around with evil keybinds:
;; https://www.reddit.com/r/evilmode/comments/323xh1/is_it_possible_to_use_vi_keybindings_everywhere/

;; I still want to figure out how to break up this one massive config into many,
;; but for now, I give up.
;; (require 'my-evil-config)

(global-evil-leader-mode) ; per the evil-leader readme, this should be enabled first, but might not actually have an effect
(evil-mode t)

;; setup leader key stuff
;; let's stick with \ for now, but , is another common choice
;;(evil-leader/set-leader ",")
;; It requires remapping next selection, but ; is also nice.
;;(evil-leader/set-leader ";")
;; But why not use all three? TODO: Write evil-multileader
(evil-leader/set-key
  "bb" 'ibuffer
  "bd" 'dired
  "df" 'describe-function
  "dk" 'describe-key
  "dv" 'describe-variable
  "g" 'magit-status
  "k" 'kill-buffer
  "m" 'linum-mode
  "n" 'linum-relative-mode
  "pi" 'package-install
  "pl" 'package-list-packages
  "r" 'revert-buffer
  "sb" 'eval-buffer
  "ss" 'eval-last-sexp
  ; Not sure this next one is a good idea - \z works to enter emacs mode, but not to leave it.
  "wf" 'toggle-frame-fullscreen
  "wn" 'make-frame
  "z" 'evil-emacs-state
  "[" 'keyboard-quit)

(setq evil-leader/in-all-states 1)

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

; Initial mode setting.
;(evil-set-initial-state 'ibuffer-mode 'normal)

(defun set-evil-initial-mode (start-mode &rest mode-list)
  "Ensure every package listed is installed. If a package is not installed, try to install it.
   Returns a list of installed packages, or nil if every package is skipped."
  (mapcar
   (lambda (mode) (evil-set-initial-state mode start-mode))
   mode-list))

(set-evil-initial-mode 'normal
                       'ibuffer-mode
                       'package-menu-mode
                       'completion-list-mode
                       )

(provide 'config-evil)
