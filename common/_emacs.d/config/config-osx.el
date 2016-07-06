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
