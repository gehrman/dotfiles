;;; config-powerline --- powerline configuration
;; without the pain of the vim install. ;;

;;; Commentary:
;; https://github.com/powerline/fonts
;; http://emacs.stackexchange.com/questions/281/how-do-i-get-a-fancier-mode-line-that-uses-solid-colors-and-triangles

;;; Code:
(ensure-package-installed
 'telephone-line
 )

(setq telephone-line-evil-use-short-tag t)
(telephone-line-defsegment* my/position-segment ()
  "Return string giving cursor position."
  (telephone-line-raw "L%l C%c"))
(telephone-line-defsegment* my/buffer-name-segment ()
  "Return a buffer-name string, prefixed with ! if modified."
  (telephone-line-raw
   (if (buffer-modified-p) (concat "!" (buffer-name)) (buffer-name))))

(setq telephone-line-lhs
      '((evil my/position-segment)
        (accent telephone-line-major-mode-segment)
        (nil telephone-line-projectile-segment)))

(setq telephone-line-center-lhs
      '((evil my/buffer-name-segment)))

(setq telephone-line-rhs
      '((nil telephone-line-vc-segment)
        (accent telephone-line-flycheck-segment)
        (evil telephone-line-evil-tag-segment)))

(telephone-line-mode 1)

(provide 'config-powerline)
;;; config-powerline ends here
