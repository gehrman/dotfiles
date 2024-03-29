;;; config-misc --- Miscellaneous setup.

;;; Commentary:
;; This file is for (package) configuration that don't really fit elsewhere.
;; More general/minimal necessary configuration should go in init.el, but
;; clearly there's still some overlap between the two.

;;; Code:
(require 'package-tools)
(require 'config-company)

(ensure-package-installed
 'elfeed
 'emms
 'groovy-mode
 ;;'emms-mark-ext
 ;;'emms-player-simple-mpv
 ;;'emms-soundcloud
 ;;'emms-state
 'pianobar
 'pollen-mode
 'company-pollen
 )

(setq desktop-save-mode 1)
(setq desktop-save t)

(add-to-list 'auto-mode-alist '("Jenkinsfile$" . nginx-mode))

(require 'pianobar)
(setq pianobar-username "gehrman@gmail.com")

(require 'emms-setup)
;; (emms-minimalistic)  ;; Only invis playlists and basic playback
(emms-all) ;; setup all features
(emms-default-players)
(setq emms-player-vlc-command-name
      "/Applications/VLC.app/Contents/MacOS/VLC")


;; Eventually we might need a config-system, but for now shell script stuff goes here.
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Set and configure recentf mode - remembers recently opened files across
;; sessions.
(setq recentf-save-file (concat user-emacs-directory "-recentfiles"))
(require 'recentf)
(recentf-mode 1)
;(global-set-key "" 'recentf-open-files) ;Bind key to open file list.

;; Projectile/Tags stuff
(defun regenerate-tags ()
  "Function to regenerate ctags."
  (interactive)
  (let ((tags-directory (directory-file-name (projectile-project-root))))
    (shell-command
     (format "ctags -f %s -e -R %s" tags-file-name tags-directory))))

(defun gcn () "Go to the code directory."
  (interactive)
  (helm-find-files-1 "~/Devel/vtrack/"))
(evil-leader/set-key "eg" 'gcn)
;; How to undefine a key given a key-map:
;; (define-key evil-normal-state-local-map ",e" nil)
;; TODO - defun this as 'gbe-unset-key

;; Open worktree
;; (defun open-code-dir ()
;;   "Open a directory in code dir"
;;   (interactive)
;;   ())

;; (use-package ctags-update
;;   :ensure t
;;   :config
;;   (progn
;;     (add-hook 'python-mode-hook 'turn-on-ctags-auto-update-mode)))

;; load avro IDL files in IDL mode, and proto/schema files in json mode
(add-to-list 'auto-mode-alist '("\\.avdl$" . idl-mode))
(add-to-list 'auto-mode-alist '("\\.avpr$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.avsc$" . json-mode))

;; Configure Pollen
(add-to-list 'auto-mode-alist '("\\.pp$" . pollen-mode))

(provide 'config-misc)
;;; config-misc.el ends here
