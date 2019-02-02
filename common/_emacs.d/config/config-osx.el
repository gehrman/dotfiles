;;; config-osx --- OS X specific customizations and settings.

;;; Commentary:
;; Emacs 25 _does_ seem much less prone to falling over on a :q; that plus
;; updating the fashion in which the server is launched has obliviated most of
;; the problems this file was solving. Note that using exec-path-from-shell
;; requires a fully specified path - no exporting PATH=/new:$PATH.

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'exec-path-from-shell)

(require 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'config-osx)
;;; config-osx.el ends here
