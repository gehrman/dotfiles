;;; config-webdev --- Package config for web development.

;;; Commentary:
;; Not much here for now, beyond sourcing the various http stuff.

;;; Code:
(require 'package-tools)

(ensure-package-installed
 'restclient
 'restclient-helm
 'restclient-test
 'ob-http
 'ob-restclient
 'know-your-http-well
 'company-restclient
 'json-mode
 'json-reformat
 'tagedit ;Edit HTML tags like the sexps they are.
 'nginx-mode
 ;;https://github.com/skeeto/skewer-mode
 ;; there's also swank-js + slime
 ;;'skewer-less
 ;;'skewer-mode
 ;;'skewer-reload-stylesheets
 )

(require 'evil)
(require 'evil-leader)
(require 'tagedit)

;; HTTP Interaction:
(require 'restclient)

;; HTTP completions
(require 'know-your-http-well)


;; Missing function in the restclient definitions.
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
;; <leader>ss is common enough that binding it to C-return might be worthwhile.
(evil-define-key 'normal restclient-mode-map (kbd "<return>") 'restclient-http-send-current-stay-in-window)
(evil-define-key 'normal restclient-mode-map (kbd "C-<return>") 'restclient-http-send-current-stay-in-window)
(evil-define-key 'insert restclient-mode-map (kbd "C-<return>") 'restclient-http-send-current-stay-in-window)
(evil-define-key 'normal restclient-mode-map (kbd "<kp-enter>") 'restclient-http-send-current-stay-in-window)
(evil-define-key 'normal restclient-mode-map (kbd "C-<enter>") 'restclient-http-send-current-stay-in-window)
(evil-define-key 'insert restclient-mode-map (kbd "C-<enter>") 'restclient-http-send-current-stay-in-window)
(evil-define-key 'insert restclient-mode-map (kbd "c") 'self-insert-command)

;;; Javascript
;; So, right Clojure for the Brave and True calls for js-mode while the
;; default is js3-mode. Not sure what the difference is, but we'll go
;; with the Brave and True setting for now.
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;; Indentation settings!
(setq js-indent-level 2)

;; Brave and True sets subword mode for Javascript and HTML, however evil
;; does not respect subwords. Leaving this commented out since I generally
;; use FT (or other vim motions) to deal with this rather than emacs
;; native commands that respect the CamelCase -> Camel Case split.
;(add-hook 'js-mode-hook 'subword-mode)
;(add-hook 'html-mode-hook 'subword-mode)
;(add-hook 'coffee-mode-hook 'subword-mode)

;; Coffeescript
(add-to-list 'auto-mode-alist '("\\.coffee.erb$" . coffee-mode))
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook
 'coffee-mode-hook
 (defun coffee-mode-newline-and-indent ()
   (define-key coffee-mode-map "\C-j" 'coffee-newline-and-indent)
   (setq coffee-cleanup-whitespace nil)))
(setq coffee-tab-width 2)

;; HTML
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))

;; JSON
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(defun beautify-json-buffer ()
  "Reformat json buffer."
  (interactive)
  (save-excursion
    (shell-command-on-region
     (point-min)
     (point-max)
     "jq --indent 2 \".\""
     (buffer-name)
     t)))

;; Nginx
(add-to-list 'auto-mode-alist '("\\.server$" . nginx-mode))

(provide 'config-webdev)
;;; config-webdev.el ends here
