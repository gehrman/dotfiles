;;; config-latex --- Configuration specific to working with TeX/LaTeX.
;;; Commentary:

;;; Code:
;; (ensure-package-installed
;;  'cdlatex
;;  'latex-math-preview
;;  'latex-pretty-symbols
;;  'latex-extra
;;  'latex-preview-pane
;;  'magic-latex-buffer
;;  'math-symbol-lists
;;  'px ; inline latex preview
;;  )

(defun insert-environment ()
  "Insert a begin-end block."
  (interactive)
  (let* ((env (read-string "Environment Name: "))
         (begin (concat "\\begin{" env "}"))
         (end (concat "\\end{" env "}")))
    (save-excursion (insert (concat begin "\n" end)))))
(define-key 'latex-mode-map (kbd "s-j") 'insert-environment)
(define-key 'tex-mode-map (kbd "s-j") 'insert-environment)

;; https://www.emacswiki.org/emacs/PrettySymbol
;;(require 'latex-pretty-symbols)
(provide 'config-latex)
;;; config-latex.el ends here
