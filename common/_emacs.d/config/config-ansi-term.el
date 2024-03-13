;;; Package -- Summary
;;; Commentary:

;;; Code:
;; TODO: make this mode-specific
;; TODO: manage states --
;;       line-mode => evil-emacs-state + move point to right place
;;       char-mode => evil-normal-state
(defun gbe/launch-zsh-term ()
  "Launch zsh in an ansi term."
  (interactive)
  (ansi-term "/bin/zsh"))
(define-key global-map (kbd "s-<return>") 'gbe/launch-zsh-term)

;; These and the lower need to load as a hook so that term-raw-map is defined
(add-hook
 'term-mode-hook
 (lambda ()
   (define-key term-raw-map (kbd "s-i") 'term-paste)
   (define-key term-raw-map (kbd "s-v") 'term-paste)
   (define-key term-raw-map (kbd "s-o") 'term-line-mode)
   (define-key term-raw-map (kbd "s-p") 'term-char-mode)

   ;; Normally, term-raw-map passes C-w in raw so that backward-delete-word
   ;; Works as expected in the terminal. However, it's more useful to have
   ;; the usual window manouvering working, we'll unset it to get access to the
   ;; evil motion binds.
   (define-key term-raw-map  (kbd "C-w") nil)

   ;; It's rare enough that I need to type a comma in a terminal that it's
   ;; worth it to unbind it to get access to the leader key
   (define-key term-raw-map (kbd ",") nil)

   ;; We also want to move vim-mode to Magit's C-; binding so that we can kill
   ;; the shell with C-d like normal
   (define-key term-raw-map  (kbd "C-;") 'evil-normal-state)
   (define-key term-raw-map  (kbd "C-d") 'kill-this-buffer)))

(provide 'config-ansi-term)
;;; config-ansi-term.el ends here
