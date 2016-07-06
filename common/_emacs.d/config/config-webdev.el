;;; config-webdev --- Package config for web development.

;;; Commentary:
;; Not much here for now, beyond sourcing the various http stuff.

;;; Code:
(require 'restclient)

;; Missing function in the restclient definitions.
;;;###autoload
(defun restclient-http-send-current-raw-and-stay-in-window ()
  "Send current request and keep focus in request window."
  (interactive)
  (restclient-http-send-current t t))

;; Allow ob-http and ob-restclient to work.
(org-babel-do-load-languages
 'org-babel-load-languages
 (cons '(http . t) (cons '(restclient . t) org-babel-load-languages)))


;;; Evil Keybinds for restclient-mode
(evil-leader/set-key-for-mode 'restclient-mode
  "v" 'restclient-http-send-current-stay-in-window
  "ss" 'restclient-http-send-current-stay-in-window
  "V" 'restclient-http-send-current-raw-and-stay-in-window
  "sS" 'restclient-http-send-current-stay-in-window
  "c" 'restclient-http-send-current
  "C" 'restclient-http-send-current-raw
  "j" 'restclient-jump-next
  "k" 'restclient-jump-prev
  "m" 'restclient-mark-current
  "r" 'restclient-copy-curl-command
  "g" 'helm-restclient
  "tb" 'restclient-test-buffer
  "tt" 'restclient-test-current
  "tm" 'restclient-test-mode
  "tj" 'restclient-test-next-error
  "tk" 'restclient-test-previous-error
  )

(provide 'config-webdev)
;;; config-webdev.el ends here
