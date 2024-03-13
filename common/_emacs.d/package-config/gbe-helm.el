;;; gbe-helm --- Configuration for Helm completions

;;; Commentary:
;;; Helm is complicated, and I don't like it, but for some reason I switched to
;;; it over IDO, and now I'm used to completions in a separate window even though
;;; it doesn't work that well
;;; See:
;;;   * https://github.com/thierryvolpiatto/emacs-config/blob/main/init-helm.el
;;;   * https://github.com/emacs-helm/helm/wiki#configure

;;; Code:
(use-package helm
  :straight t
  :config
  (helm-mode 1)
  (setq helm-fuzzy-match t))

;; This isn't really a helm thing, it's how we run ag searches...
(use-package helm-ag
  :straight t)

(provide 'gbe-helm)
;;; gbe-helm.el ends here
