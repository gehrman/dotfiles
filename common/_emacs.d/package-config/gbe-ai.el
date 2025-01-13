;;; gbe-ai -- Configuration for AI tools

;;; Commentary:
;; Right now, this is probably all going to be Github Copilot
;;
;; Disabled as of 4/24/2024 because the little bit of magic doesn't come close to
;; the breaking of flow it causes, the absolute garbage it generates in comments,
;; and the fact it causes Emacs to often hang for 10 or more seconds. Basically,
;; this shit was actively making me stupider and a worse developer. Burn it to the
;; ground.

;;; Code:
;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
;;   :hook (prog-mode . copilot-mode)
;;   :config
;;   ;(copilot-install-server)
;;   (define-key evil-insert-state-map (kbd "<backtab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-next-completion)
;;   (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

                                        ;(define-key 'copilot-completion-map (kbd "C-`") 'copilot-next-completion)

;; (provide 'gbe-ai)
;;; gbe-ai.el ends here
