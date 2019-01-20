(when (configuration-layer/package-usedp 'flyspell)
  (defun lang/change-dictionary ()
	  " change dictionary."
	  (interactive)
	  (print
		 (cond
		  ((string= ispell-dictionary "en_US") (setq
			                                      ispell-dictionary "pt_BR"
			                                      langtool-default-language "pt-BR"
		                                        ))
		  (t (setq
			    ispell-dictionary "en_US"
			    langtool-default-language "en-US"
		      ))
      ))))
