;;; config-company --- Global Company-mode configuration
;;; Commentary:
;; Fr now, this is probably going to be just initialization and keybinds
;; ...you know, or not.

;;; Code:
(require 'package-tools)
(ensure-package-installed
 'company
 'company-flx
 )

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
;; Doubt that we actually need to do the eval-after-load, but w/e.
(with-eval-after-load
    'company
  (company-flx-mode t)
  )


;; Add a missing completion function.
(defun company-complete-common-and-cycle (&optional arg)
  "Insert the common part of all candidates and select the next one.

With ARG, move by that many elements."
  (interactive "p")
  (when (company-manual-begin)
    (call-interactively 'company-complete-common)
    (let ((company-selection-wrap-around t)
          (current-prefix-arg arg))
      (call-interactively 'company-select-next))))


;; Based on https://github.com/company-mode/company-mode/issues/75, we're
;; rebinding both (kbd "TAB") and [tab] because they're different in GUI versus
;; terminal mode.
(eval-after-load 'company
  '(progn
     (setq company-selection-wrap-around t)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-and-cycle)
     (define-key company-active-map (kbd "TAB") 'company-complete-common-and-cycle)
     (define-key company-active-map (kbd "S-<tab>") 'company-select-previous-or-abort)
     (define-key company-active-map (kbd "TAB") 'company-select-previous-or-abort)
     ))

;; The following code should allow for tab-initiation of completion while
;; preserving it's use for indentation:
;; https://emacs.stackexchange.com/questions/12441/is-it-possible-to-start-company-completion-without-a-prefix
;; but, so far, this seems unnecessary.


(provide 'config-company)

;;; config-company.el ends here
