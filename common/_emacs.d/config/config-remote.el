;;; config-remote -- Config for dealing with remote systems and TRAMP
;;; Commentary:
;;

;;; Code:
(defun open-remote-devel-dir ()
  "Do a gcn."
  (interactive)
  (find-file "/ssh:dopres:/wayfair/home/gehrman/dev"))

(provide 'config-remote)
;;; config-remote ends here
