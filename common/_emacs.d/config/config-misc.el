;;; config-misc --- Miscellaneous setup.

;;; Commentary:
;; This file is for (package) configuration that don't really fit elsewhere.
;; More general/minimal necessary configuration should go in init.el, but
;; clearly there's still some overlap between the two.

;;; Code:
(require 'pianobar)
(setq pianobar-username "gehrman@gmail.com")

;; Eventually we might need a config-system, but for now shell script stuff goes here.
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Set and configure recentf mode - remembers recently opened files across
;; sessions.
(setq recentf-save-file (concat user-emacs-directory "-recentfiles"))
(require 'recentf)
(recentf-mode 1)
;(global-set-key "" 'recentf-open-files) ;Bind key to open file list.

(provide 'config-misc)
;;; config-misc.el ends here
