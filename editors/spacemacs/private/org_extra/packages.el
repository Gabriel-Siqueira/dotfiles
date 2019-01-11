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
    org-journal
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
  )

(defun org_extra/init-org-journal ()
  (use-package org-journal
    :defer t
    :commands (org-journal-new-entry org-journal-search-forever)
    :init
    (progn
      (spacemacs/declare-prefix "aoj" "org-journal")
		  (setq org-journal-dir "~/Dropbox/Org/journal")
      (spacemacs/set-leader-keys
        "aojj" 'org-journal-new-entry
        "aojs" 'org-journal-search-forever)

      (spacemacs/set-leader-keys-for-major-mode 'calendar-mode
        "r" 'org-journal-read-entry
        "i" 'org-journal-new-date-entry
        "n" 'org-journal-next-entry
        "p" 'org-journal-previous-entry
        "s" 'org-journal-search-forever
        "w" 'org-journal-search-calendar-week
        "m" 'org-journal-search-calendar-month
        "y" 'org-journal-search-calendar-year)

      (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
        "j" 'org-journal-new-entry
        "n" 'org-journal-open-next-entry
        "p" 'org-journal-open-previous-entry))))

(defun org_extra/post-init-org-agenda ()
  (setq org-agenda-files (list "~/Dropbox/Org/"))
  (setq org-agenda-start-on-weekday 0) ; week starts on sunday
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-custom-commands '(
    ("x" "main view" (
      (tags "urgente|pin")
      (agenda "" ((org-agenda-span 1)))
      (todo "TODO|TODAY")
      )))))

(defun org_extra/init-org-super-agenda ()
  (use-package org-super-agenda
	  :defer t
	  :init (org-super-agenda-mode)
	  :config
		(setq org-super-agenda-groups '(
      (:name "Urgente" :tag "urgente" :deadline today :order 1
              :face (:background "red" :foreground "yellow"))
      (:name "Pin" :tag "pin" :order 2)
      (:name "Today" :todo "TODAY"
              :face (:background "black" :foreground "yellow") :order 3)
      (:name "Study" :tag "study"
              :face (:background "black" :foreground "yellow") :order 5)
      (:name "Grid" :and (:time-grid t :not (:todo "TODO"))
              :face (:foreground "RosyBrown1") :order 4)
      (:name "Today" :date today :scheduled past
              :face (:background "black" :foreground "yellow") :order 5)
      (:name "Todo" :todo "TODO" :order 6)
      ))
	  (setq org-deadline-warning-days 0)
    ))

;;; packages.el ends here
