(when (configuration-layer/package-usedp 'flyspell)
  (defun lang/change-dictionary ()
	  " change dictionary."
	  (interactive)
      (cond
        ((string= ispell-dictionary "pt_BR")
          (setq
          ispell-dictionary "en_US"
          langtool-default-language "en-US")
          )
        (t
          (setq
          ispell-dictionary "pt_BR"
          langtool-default-language "pt-BR"))
      )
      (ispell-internal-change-dictionary)
      (flyspell-buffer)
      (print ispell-dictionary)
      ))
