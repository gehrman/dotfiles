;;; config-powerline --- powerline configuration
;; without the pain of the vim install. ;;

;;; Commentary:
;; https://github.com/powerline/fonts
;; http://emacs.stackexchange.com/questions/281/how-do-i-get-a-fancier-mode-line-that-uses-solid-colors-and-triangles

;;; Code:
(ensure-package-installed
 'powerline
 'powerline-evil
 )

(require 'powerline)
(require 'powerline-evil)
;(powerline-default-theme)
;(powerline-vim-theme)
;(powerline-evil-vim-theme)
;(powerline-evil-vim-color-theme)
(powerline-evil-center-color-theme)
(display-time)

(provide 'config-powerline)
;;; config-powerline ends here
