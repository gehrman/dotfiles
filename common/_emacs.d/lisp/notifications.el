;;; notifications --- Send OSX notifications

;;; Commentary:
;; Utility to send system notifications from Emacs. While this was originally
;; intended to integrate with Outlook, right now it's mostly used to alert if
;; black was not run. However, it might be interesting to try and integrate with
;; Slack or gmail/google calendar.

;;; Code:
(defun gbe/send-notification-from-system (msg &optional title)
  "Send MSG with optional TITLE."
  (interactive)
  (if title
      (start-process "gbe-notify" nil "terminal-notifier" "-title" title "-message" msg)
    (start-process "gbe-notify" nil "terminal-notifier" "-message" msg)))

(defun gbe/send-notification-from-emacs (msg)
  "Send MSG as a notification from Emacs."
    (start-process "gbe-notify" nil
                   "terminal-notifier"
                   "-title" "Emacs"
                   "-appIcon" "/Applications/Emacs.app/Contents/Resources/Emacs.icns"
                   "-message" msg))

(provide 'notifications)
;;; notifications.el ends here
