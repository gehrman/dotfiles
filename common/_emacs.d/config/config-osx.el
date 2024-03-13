;;; config-osx --- OS X specific customizations and settings.

;;; Commentary:
;; Emacs 25 _does_ seem much less prone to falling over on a :q; that plus
;; updating the fashion in which the server is launched has obliviated most of
;; the problems this file was solving. Note that using exec-path-from-shell
;; requires a fully specified path - no exporting PATH=/new:$PATH.

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'exec-path-from-shell
 'mac-pseudo-daemon
 )

;; Prevents emacs from quitting if the last window is closed by creating a
;; hidden window to keep the app running. To kill emacs, emacs must be
;; explicitly killed via something like :qa or running 'kill-emacs.
(mac-pseudo-daemon-mode)

;; The setq is there to stop the interactivity nagging.
(require 'exec-path-from-shell)
;;(setq exec-path-from-shell-check-startup-files nil)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'config-osx)
;;; config-osx.el ends here
