;;; packages.el --- lang layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Gabriel Siqueira
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst lang-packages
  '(
    langtool
    flyspell
    ))

(defun lang/init-langtool ()
  (use-package langtool
	  :defer t
	  :custom
    (langtool-java-classpath
        "/usr/share/languagetool:/usr/share/java/languagetool/*")
		(langtool-disabled-rules '(
        "REPEATED_WORDS"
        ) "disable specific rules")
	  :init
    (spacemacs/set-leader-keys
      "Sg" 'langtool-check
      "SG" 'langtool-check-done
    )
  ))

(defun lang/post-init-flyspell ()
  (spacemacs/set-leader-keys
    "SD" 'lang/change-dictionary
    )
  )

;;; packages.el ends here
