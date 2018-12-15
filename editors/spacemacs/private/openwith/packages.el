;;; packages.el --- openwith layer packages file for Spacemacs.
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

(defconst openwith-packages
  '(openwith))

  (defun openwith/init-openwith ()
    (use-package openwith
	    :defer t
	    :init (openwith-mode t)
	    :custom
	    (openwith-associations '(
          ("\\.pdf\\'" "xdg-open" (file))
          ) "select how to open each file")
      ))
