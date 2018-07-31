;;; Package -- Summary

;;; Commentary:
;; Configure docker tools

;;; Code:
(require 'docker)
(require 'dockerfile-mode)
(require 'docker-compose-mode)
(require 'docker-tramp)

;; Start in emacs mode
(add-to-list 'evil-emacs-state-modes 'docker-image-mode)

;; This is the wrong place for this, but since docker-image-mode derives from
;; tabulated-list-mode, we're registerig it here.
(defun tablist-modes-keybinds ()
  "Set keybinds for docker modes."
  (local-set-key (kbd ",") 'ibuffer)
  )
(add-hook 'tabulated-list-mode-hook 'tablist-modes-keybinds)

(provide 'config-docker)
;;; config-docker.el ends here
