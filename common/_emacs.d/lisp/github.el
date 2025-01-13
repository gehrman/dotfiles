;;; Package -- gbe/github

;;; Commentary:

;;; Code:
(defun load-secret/load-string (path)
  "Load the secret stored at PATH."
  ())

(let ((var-b "https://github.com/<beta>...{branch}?expand=1&template=<beta>")
      (var-m "https://github.com/<main>...{branch}?expand=1&template=<main>"))
  (message var-b)
  (message var-m))

(provide 'gbe/github)
;;; github.el ends here
