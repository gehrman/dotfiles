;;; config-orgmode -- Setup org-mode.

;;; Commentary:

;;; Code:

;; Initial setup
;;(load-library "org")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Set languages available for execution in code blocks.
(require 'cl-lib)
(require 'evil)
(require 'evil-leader)

(org-babel-do-load-languages
  'org-babel-load-languages
  (cl-remove-duplicates (append org-babel-load-languages
                             '((emacs-lisp . t)(python . t)))
                     :test 'equal))

;; Org-mode config from Patrick... I really do need to learn org-mode at some point.

;; (require 'org-install)
;; (require 'ob-tangle)
;; (require 'ob-latex)

;; ; We need to load tex.el first...
;; (load "tex.el")
;; ;;(require 'org-latex)

;; (setq org-src-fontify-natively t)

;; (evil-leader/set-key-for-mode 'org-src-mode
;;   "wq" 'org-edit-src-exit
;;   "q" 'org-edit-src-abort)
;; I think the way to do this will be to add post-load advice around
;; the 'org-edit-special function to manipulate that mode's keybinds.
;; The exact way to grab the keybind map and add temporary org-src-mode
;; key binds isn't clear yet. For more details, and a partially relevant
;; example, see http://emacs.stackexchange.com/a/20397.
(add-hook
 'org-mode-hook
 (lambda ()
   (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
   (evil-define-key 'normal org-mode-map (kbd "C-\\") 'org-insert-heading)
   (evil-define-key 'insert org-mode-map (kbd "C-\\") 'org-insert-heading)
   (auto-fill-mode)))

;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c b") 'org-iswitchb)
;; (define-key global-map (kbd "C-c c") 'org-capture)

;; (add-hook 'org-mode-hook 'auto-fill-mode)
;; (add-hook 'org-mode-hook 'flyspell-mode)

;; ;; Add timestamps to completed TODOs
;; (setq org-log-done 'time)

;; (setq org-agenda-files (list "~/org/agenda.org"
;;                              "~/org/todo.org"
;;                              "~/org/journal.org"
;;                              "~/org/research.org"
;;                              "~/org/courses.org"))


;; (setq org-refile-targets (quote ((nil :maxlevel . 9)
;;                                  (org-agenda-files :maxlevel . 9))))

;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
;;          "* TODO %?\n\nCreated at %U")
;;         ("j" "Journal" entry (file+datetree "~/org/journal.org")
;;          "* %?\nEntered on %U\n  %i\n  %a")
;;         ("r" "Research" entry (file+headline "~/org/research.org" "Research"))
;;         ("c" "Courses" entry (file+headline "~/org/courses.org" "Courses"))))
;; (setq org-capture-templates
;; '(("T" "A (T)est template." entry
;;   (file+headline "notes.org" "Testing")
;;   "* %:description\n%u\n\n%c\n\n%i"
;;   :empty-lines 1)))

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

;; Let's try to set up some capturing.
(require 'org-protocol)

(provide 'config-orgmode)
;;; config-orgmode.el ends here
