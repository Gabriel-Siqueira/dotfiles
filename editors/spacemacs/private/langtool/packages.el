;;; packages.el --- %LAYER_NAME% layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: %USER_FULL_NAME% <%USER_MAIL_ADDRESS%>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst langtool-packages
  '(
    langtool
    ))

(defun langtool/init-langtool ()
  (use-package langtool
	  :defer t
	  :custom
    (langtool-java-classpath
        "/usr/share/languagetool:/usr/share/java/languagetool/*")
		(langtool-disabled-rules '(
        "REPEATED_WORDS"
        ) "disable specific rules")
	  :config
    (spacemacs/set-leader-keys
      "Sg" 'langtool-check
      "SG" 'langtool-check-done
    )
  ))

;;; packages.el ends here
