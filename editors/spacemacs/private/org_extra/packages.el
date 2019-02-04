;;; packages.el --- org_extra layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author:  <gabriel@GAMa>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst org_extra-packages
  '(
    org
    org-agenda
    org-super-agenda
    ))

(defun org_extra/post-init-org ()
  ; fold all blocks in the start
  (setq org-hide-block-startup t)
  ; lists treated as low level headlines
  (setq org-cycle-include-plain-lists 'integrate)
  ; status of checkbox include the whole hierarchy
  (setq org-checkbox-hierarchical-statistics nil)
  ; change size of latex formula's font
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))
  ; allow letter in lists
  (setq org-list-allow-alphabetical t)
  ; do not indent when demoting header
  (setq org-adapt-indentation nil)
  ; allow the use of #+Bind for set variables during export
  (setq org-export-allow-bind-keywords t)
  ; capture destination file
  (setq org-default-notes-file "~/Dropbox/Org/notes.org")
  ; capture templates
  (setq org-capture-templates
        '(
          ("t" "Todo" entry (file+headline "" "Inbox")
           "* TODO %?\n")
          ("T" "Todo on File" entry (file+headline "" "Inbox")
          "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline "" "Inbox")
           "* %?\n")
          ("j" "Journal" entry (file+datetree "~/Dropbox/Org/journal.org")
           "\n* Entered on %U\n\n- café da manha: %^{PROMPT} \n- almoço: %^{PROMPT} \n- janta: %^{PROMPT} \n- progresso: %^{PROMPT} \n- estudo: %^{PROMPT}%?\n")
          ))
  )

(defun org_extra/post-init-org-agenda ()
  (setq org-agenda-files (list "~/Dropbox/Org/activities.org"))
  (setq org-agenda-start-on-weekday 0) ; week starts on sunday
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-custom-commands '(
    ("x" "planing" (
      (tags "urgente|pin")
      (agenda "" ((org-agenda-start-day "+1d") (org-agenda-span 1)))))
    ("o" "main view" (
                      (tags "urgente|pin")
                      (agenda "" ((org-agenda-span 1)))))
    )))

(defun org_extra/init-org-super-agenda ()
  (use-package org-super-agenda
	  :defer t
	  :init (org-super-agenda-mode)
	  :config
		(setq org-super-agenda-groups '(
      (:name "Urgente" :tag "urgente" :deadline today :order 1
              :face (:background "red" :foreground "yellow"))
      (:name "Pin" :tag "pin" :order 2)
      (:name "Grid" :time-grid t
              :order 4)
      ))
	  (setq org-deadline-warning-days 0)
    ))

;;; packages.el ends here
