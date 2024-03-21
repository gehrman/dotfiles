(use-package vterm
  :straight t
  :hook (vterm-mode
         .
         (lambda ()
           (define-key vterm-mode-map (kbd "C-w") nil)
           (define-key vterm-mode-map (kbd "C-w C-w") (lambda () (interactive) (vterm-send "C-w")))
           (define-key vterm-mode-map (kbd "C-u") 'vterm--self-insert)))
  :config (define-key global-map (kbd "C-s-<return>") 'vterm))
