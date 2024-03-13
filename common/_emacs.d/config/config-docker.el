;;; Package -- Summary

;;; Commentary:
;; Configure docker tools

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'docker
 'dockerfile-mode
 'docker-compose-mode)

(require 'docker)
(require 'dockerfile-mode)
(require 'docker-compose-mode)
;;(require 'docker-tramp)  ;; tramp-container now included by default
(require 'evil)
(require 'evil-leader)

;; Start in emacs mode
(add-to-list 'evil-emacs-state-modes 'docker-image-mode)

;; This is the wrong place for this, but since docker-image-mode derives from
;; tabulated-list-mode, we're registerig it here.
(defun tablist-modes-keybinds ()
  "Set keybinds for docker modes."
  (local-set-key (kbd ",") 'ibuffer))
(add-hook 'tabulated-list-mode-hook 'tablist-modes-keybinds)

(evil-leader/set-key
  "DD" 'docker-compose)

(provide 'config-docker)
;;; config-docker.el ends here
