;; (use-package vterm
;;   :straight t
;;   :hook
;;   (vterm-mode
;;    .
;;    (lambda ()
;;      (define-key vterm-mode-map (kbd "C-d") 'vterm--self-insert)
;;      ;; C-o should run 'evil-execute-in-normal-state if not shadowed
;;      ;; ...maybe. Might need to explicitly bind it, otherwise global-map
;;      ;; gets it
;;      ;; (define-key vterm-mode-map (kbd "C-o") nil)
;;      (define-key vterm-mode-map (kbd "C-w") nil)
;;      ;;(define-key vterm-mode-map (kbd "C-w ,") 'evil-normal-state-map)
;;      (define-key vterm-mode-map (kbd "C-w C-w") (lambda () (interactive) (vterm-send "C-w")))
;;      (define-key vterm-mode-map (kbd "C-u") 'vterm--self-insert)))
;;   :config
;;   (define-key global-map (kbd "C-s-<return>") 'vterm)
;;   (setq vterm-max-scrollback 100000)
;;   (setq vterm-shell "/bin/zsh"))

;; See https://codeberg.org/akib/emacs-eat.git for install recipe and config
;; (use-package eat
;;   :straight (
;;    :url "https://codeberg.org/akib/emacs-eat.git"
;;    :host codeberg
;;    :repo "akib/emacs-eat"
;;    :files ("*.el" "dir" "*.info" "*.texi" "*.ti" ("e" "e/*"))
;;    )
;;   :config
;;   ())

;;(provide 'gbe-term)
;;; gbe-term.el ends here
