;; early-init --- Early global configuration

;;; Commentary:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html:
;; Most customizations for Emacs should be put in the normal init
;; file. See The Emacs Initialization File. However, it is sometimes
;; necessary to have customizations take effect during Emacs startup
;; earlier than the normal init file is processed. Such customizations
;; can be put in the early init file, ~/.config/emacs/early-init.el or
;; ~/.emacs.d/early-init.el. This file is loaded before the package
;; system and GUI is initialized, so in it you can customize variables
;; that affect the package initialization process, such as
;; package-enable-at-startup, package-load-list, and package-user-dir.
;; Note that variables like package-archives which only affect the
;; installation of new packages, and not the process of making
;; already-installed packages available, may be customized in the
;; regular init file. See Package Installation.

;; For now, we can't disable loading of package.el, as we need it to load all
;; of the legacy configs. Once everything has been ported to package-config
;; however, this can be disabled and straight.el can be used as the only package
;; manager.

;; Code:
;; (setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
