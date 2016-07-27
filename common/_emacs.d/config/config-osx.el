;;; config-osx --- OS X specific customizations and settings.

;;; Commentary:
;; The most pressing fix in here is working around the rather pernicious bug
;; where quitting (at the least via a :q, which is where I notice it most, but
;; possibly via other means too) a maximize Emacs gui connected to an Emacs
;; daemon brings down the daemon and all other clients, not just the gui.
;; ...Yeah, not a good time.
;; The bug seems to be specific to Emacs 24, but for now, I don't want to
;; switch to dev-emacs, wrapping the problematic quit functions is the better
;; option.

;;; Code:
(defun demaximize-frame ()
  "Unmaximize the current frame if maximized, otherwise do nothing."
  (if (equal (assoc 'fullscreen (frame-parameters))
             (cons 'fullscreen 'fullboth))
      (toggle-frame-fullscreen)))
(defun before-evil-quit (orig-fun &rest args)
  "Wrap ORIG-FUN and ARGS to work around OS X / Emacs 24 fullscreen quit crash."
  (demaximize-frame))
(advice-add 'evil-quit :before #'before-evil-quit)

;; See https://github.com/purcell/exec-path-from-shell for package description.
;; It's not clear if this is necessary, but it's quite possible the answer to
;; at least some of the weirdness around paths I've seen in OS X.
;(ensure-package-installed 'exec-path-from-shell)
;; An alternative suggested in the ag.el documentation is
; (defun set-exec-path-from-shell-PATH ()
;   "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
; This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
;   (interactive)
;   (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;     (setenv "PATH" path-from-shell)
;     (setq exec-path (split-string path-from-shell path-separator))))
; (set-exec-path-from-shell-PATH)
;; Not that since we're already setting 'exec-path below, it's likely the issue is in
;; (setenv/getenv)'s PATH variable instead. This may, or may not, be related to the insanity
;; with TRAMP-git. Oy vey.
;; For now, we just manually set the 'ag-executable:
(setq ag-executable "/usr/local/bin/ag")

(setq exec-path
      (quote
       ("/Users/gehrman/.bin"
        "/usr/local/bin"
        "/usr/bin"
        "/bin"
        "/usr/sbin"
        "/sbin"
        "/usr/local/Cellar/emacs/24.5/libexec/emacs/24.5/x86_64-apple-darwin15.5.0")))



(provide 'config-osx)
;;; config-osx.el ends here
