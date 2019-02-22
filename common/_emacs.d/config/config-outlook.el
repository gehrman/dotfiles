;;; config-outlook --- Email and calendar settings.

;;; Commentary:
;; I'm tired of Mac Outlook's shitty notifications system, so let's ten-ton
;; hammer this thing and solve it with elisp.

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'excorporate)

;; Eventually the should be spun off into their own little util. Especially
;; since the first use I'm making of them isn't for outlook (since that's gonna
;; take) some real effort, but for to warn if black isn't being run.
(defun send-notification-from-system (msg &optional title)
  "Send MSG with optional TITLE."
  (interactive)
  (if title
    (start-process "gbe-notify" nil "terminal-notifier" "-title" title "-message" msg)
    (start-process "gbe-notify" nil "terminal-notifier" "-message" msg)))

(defun send-notification-from-emacs (msg)
  "Send MSG as a notification from Emacs."
    (start-process "gbe-notify" nil
                   "terminal-notifier"
                   "-title" "Emacs"
                   "-appIcon" "/Applications/Emacs.app/Contents/Resources/Emacs.icns"
                   "-message" msg))

(provide 'config-outlook)
;;; config-outlook.el ends here
