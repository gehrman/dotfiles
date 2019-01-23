;;; config-modeline --- modeline configuration

;;; Commentary:
;; https://github.com/powerline/fonts

;;; Code:
(ensure-package-installed
 'telephone-line
 )

(setq telephone-line-primary-left-separator 'telephone-line-nil
      telephone-line-primary-right-separator 'telephone-line-nil
      telephone-line-secondary-left-separator 'telephone-line-nil
      telephone-line-secondary-right-separator 'telephone-line-nil)

(setq telephone-line-evil-use-short-tag t)
(telephone-line-defsegment* my/position-segment ()
  "Return string giving cursor position."
  (telephone-line-raw "L%l C%c"))
(telephone-line-defsegment* my/buffer-name-segment ()
  "Return a buffer-name string, prefixed with ! if modified."
  (telephone-line-raw
   (if (buffer-modified-p) (concat "!" (buffer-name)) (buffer-name))))
(telephone-line-defsegment* my/blank-segment () (telephone-line-raw ""))

(setq telephone-line-lhs
      '((evil my/position-segment)
        (accent telephone-line-major-mode-segment)
        (nil telephone-line-projectile-segment)))

(setq telephone-line-center-lhs
      '((nil my/blank-segment)(evil my/blank-segment)))
(setq telephone-line-center-rhs
      '((evil my/buffer-name-segment)(nil my/blank-segment)))

(setq telephone-line-rhs
      '((nil telephone-line-vc-segment)
        (accent telephone-line-flycheck-segment)
        (evil telephone-line-evil-tag-segment)))

(telephone-line-mode 1)

(provide 'config-modeline)
;;; config-modeline ends here
