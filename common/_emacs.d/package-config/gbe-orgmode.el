;;; gbe-orgmode --- Terminals in emacs

;;; Commentary:
;; Org Mode configuration

;;; Code:

(defun gbe/gaming/shadowrun/job-template ()
  "Job template for SR capture."
  (concat
   "*** %^{Job Description} \n"
   "- Fixer: %^{fixer|Alvis|Jaws|other||}\n"
   "- Johnson: %^{johnson}\n"
   "- Primary Objective: %^{primary objective}\n"
   "- Secondary Objectives: %^{secondary objectives}\n"
   "- Reward: %^{reward|K Karma, Y ¥|K Karma|Y ¥|}\n"
   "- Paydata: %^{paydata}\n"
   "- Requirements: %^{requirements}\n"
   "\n"
   "  %?"))
(defun gbe/gaming/shadowrun/character-template ()
  "Job template for SR capture."
  (concat
   "** %?%^{name} \n"
   "- Role: %^{job}\n"
   "- Citizenship: %^{citizenship}\n"
   "- Metatype: %^{metatype}\n"
   "- Age: %^{age}"))

;; (use-package org
;;   ;; Install:
;;   ;;'org-sticky-header
;;   ;;'org-superstar
;;   :straight t
;;   :hook
;;   (org-mode . (lambda ()
;;                 ;; (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
;;                 ;; (evil-define-key 'normal org-mode-map (kbd "C-\\") 'org-insert-heading)
;;                 ;; (evil-define-key 'insert org-mode-map (kbd "C-\\") 'org-insert-heading)
;;                 (define-key org-mode-map (kbd "C-l") 'org-do-demote)
;;                 (define-key org-mode-map (kbd "C-h") 'org-do-promote)))
;;   (org-mode . auto-fill-mode)

;;   ;;:custom
;;   :config
;;   (define-key evil-normal-state-map (kbd "C-t") 'org-capture)
;;   (define-key evil-insert-state-map (kbd "C-t") 'org-capture)
;;   (define-key global-map (kbd "C-t") 'org-capture)
;;   (define-key gbe/C-s-map (kbd "C-d") 'org-capture)
;;   (setq org-capture-templates
;;         '(
;;           ("h" "Shadowrun Hawaii (non-jumping)")
;;           ("hj" "Jobs" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/runs.org" "Run Ideas" "Jobs")
;;            (function gbe/gaming/shadowrun/job-template)
;;            :jump-to-captured nil
;;            :empty-lines 2)
;;           ("hr" "Runs" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/runs.org" "Run Ideas" "Runs")
;;            (function gbe/gaming/shadowrun/job-template)
;;            :jump-to-captured nil
;;            :empty-lines 2)

;;           ("hc" "Shadowrun Hawaii Characters (non-jumping)")
;;           ("hcc" "Contacts" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/characters.org.org" "Contacts")
;;            (function gbe/gaming/shadowrun/character-template)
;;            :jump-to-captured nil
;;            :empty-lines 2)
;;           ("hcm" "Criminal" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/characters.org.org" "Criminal")
;;            (function gbe/gaming/shadowrun/character-template)
;;            :jump-to-captured nil
;;            :empty-lines 2)
;;           ("hcf" "Fixers" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/characters.org.org" "Fixers")
;;            (function gbe/gaming/shadowrun/character-template)
;;            :jump-to-captured nil
;;            :empty-lines 2)
;;           ("hcj" "Johnsons" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/characters.org.org" "Johnsons")
;;            (function gbe/gaming/shadowrun/character-template)
;;            :jump-to-captured nil
;;            :empty-lines 2)

;;           ("H" "Shadowrun Hawaii (jumping)")
;;           ("Hj" "Jobs" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/runs.org" "Run Ideas" "Jobs")
;;            (function gbe/gaming/shadowrun/job-template)
;;            :jump-to-captured 't
;;            :empty-lines 2)
;;           ("Hr" "Runs" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/runs.org" "Run Ideas" "Runs")
;;            (function gbe/gaming/shadowrun/job-template)
;;            :jump-to-captured 't
;;            :empty-lines 2)

;;           ("Hc" "Shadowrun Hawaii NPCs (jumping)")
;;           ("Hcc" "Contacts" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/characters.org.org" "Contacts")
;;            (function gbe/gaming/shadowrun/job-template)
;;            :jump-to-captured 't
;;            :empty-lines 2)
;;           ("Hcm" "Criminal" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/characters.org.org" "Criminal")
;;            (function gbe/gaming/shadowrun/job-template)
;;            :jump-to-captured 't
;;            :empty-lines 2)
;;           ("Hcf" "Fixers" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/characters.org.org" "Fixers")
;;            (function gbe/gaming/shadowrun/job-template)
;;            :jump-to-captured 't
;;            :empty-lines 2)
;;           ("Hcj" "Johnsons" entry
;;            (file+olp "~/Gaming/Shadowrun Hawaii/characters.org.org" "Johnsons")
;;            (function gbe/gaming/shadowrun/job-template)
;;            :jump-to-captured 't
;;            :empty-lines 2)

;;           ;("H" "Shadowrun Hawaii captures (non-jumping)")
;;           ;("Hj" "Jobs capture" entry (file+headline "~/Gaming/Shadowrun Hawaii/runs.org" "Jobs"))
;;           ;("Hr" "Runs capture" entry (file+headline "~/Gaming/Shadowrun Hawaii/runs.org" "Runs"))
;;           )))
;; (setq org-capture-templates '())

(provide 'gbe-orgmode)
;;; gbe-orgmode.el ends here
