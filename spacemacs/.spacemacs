;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     javascript
     yaml
     sql
     vimscript
     html
     ;; main
     auto-completion
     git
     helm
     org
     (shell :variables shell-default-height 30 shell-default-position 'bottom)
     spell-checking
     syntax-checking
     ;; languages and file types
     bibtex
     c-c++
     emacs-lisp
     haskell
     latex
     markdown
     python
     ;; extra
     chrome
     emoji
     evil-snipe
     fasd
     games
     gtags
     ranger
     finance
     ;; local
     (org_extra :location local)
     (lang :location local)
     (openwith :location local)
     )
   dotspacemacs-additional-packages
   '(
     org-ref
     )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(org-bullets)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(
                         monokai
                         sanityinc-solarized-dark
                         light-soap
                         dichromacy
                        )
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts t
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 1.5
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'relative
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
    "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
    "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
	;; Isolate kill ring
	(setq interprogram-cut-function nil)
	(setq interprogram-paste-function nil)
	;; change mode line separators
	(setq powerline-default-separator 'bar)
	;; bibtex paths
	(setq bibtex-completion-bibliography '("~/Dropbox/Ref/ref.bib")
		bibtex-completion-library-path '("~/Dropbox/Ref/pdfs/")
		bibtex-completion-notes-path "~/Dropbox/Ref/ref.org")
	;; bibtex search in tag field
	(setq bibtex-completion-additional-search-fields '(tags))
	;; visual lines
	(global-visual-line-mode 1)
	(evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
	(evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
	;; indent with tabs
	(setq indent-tabs-mode t)
    ;; size of tab
    (setq-default tab-width 4)
    (setq evil-shift-width 4)
    (setq c-basic-offset 4)
    ;; backup file
    (setq backup-directory-alist `(("." "~/random/emacs_files" "~/Documents/emacs_files" "~/random")))
    (setq version-control t    ; Use version numbers for backups
          delete-old-versions t  ; don't ask if can delet old versions
          kept-new-versions 8)   ; Number of newest versions to keepenable-undo-in-region nil)
    ;; auto-fill-mode on text/tex buffers
    (add-hook 'text-mode-hook #'spacemacs/toggle-auto-fill-mode-off)
	(setq latex-enable-auto-fill nil)

    ;; =================== Layer Specific Configs =================================

    ;; ------------------- Haskell ---------------------------------
    (setq-default dotspacemacs-configuration-layers
                  '((haskell :variables haskell-enable-hindent-style "johan-tibell")))
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-to-list 'exec-path "~/.local/bin/")
    (setq haskell-font-lock-symbols t)
    (setq-default dotspacemacs-configuration-layers
                  '(auto-completion
                    (haskell :variables haskell-completion-backend 'intero)))

    ;; ------------------- Latex ---------------------------------
    (latex :variables latex-enable-magic t)
    ;; use make to compile
    ;; (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern tern coffee-mode sql-indent spinner mmm-mode evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-snipe evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state iedit evil-exchange evil-ediff evil-args evil-anzu anzu evil undo-tree auctex adaptive-wrap mmt powerline ht pdf-tools key-chord ivy tablist org-category-capture alert log4e gntp org-plus-contrib magit-popup magit hydra lv dash-functional htmlize parent-mode projectile request helm-bibtex parsebib haml-mode ham-mode markdown-mode html-to-markdown gitignore-mode flyspell-correct pos-tip flycheck pkg-info epl flx highlight transient git-commit with-editor web-completion-data ghc haskell-mode company bind-key biblio biblio-core yasnippet packed anaconda-mode pythonic f dash s helm avy helm-core async auto-complete popup paradox goto-chg company-auctex yapfify yaml-mode xterm-color ws-butler winum which-key web-mode volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package typit toc-org tagedit sudoku spaceline smeargle smartparens slim-mode shell-pop scss-mode sass-mode restart-emacs ranger rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode pcre2el pacmacs origami orgit org-super-agenda org-ref org-projectile org-present org-pomodoro org-mime org-download openwith open-junk-file neotree multi-term move-text monokai-theme markdown-toc magit-gitflow macrostep lorem-ipsum live-py-mode linum-relative link-hint ledger-mode langtool intero indent-guide hy-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md ggtags fuzzy flyspell-correct-helm flymd flycheck-pos-tip flycheck-ledger flycheck-haskell flx-ido fill-column-indicator fasd fancy-battery eyebrowse expand-region exec-path-from-shell evil-unimpaired evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-escape eval-sexp-fu eshell-z eshell-prompt-extras esh-help emoji-cheat-sheet-plus emmet-mode elisp-slime-nav edit-server dumb-jump disaster diminish define-word dactyl-mode cython-mode company-web company-statistics company-ghci company-ghc company-emoji company-cabal company-c-headers company-anaconda column-enforce-mode cmm-mode cmake-mode clean-aindent-mode clang-format bind-map auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell 2048-game)))
 '(safe-local-variable-values (quote ((origami-fold-style . triple-braces)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C")))))
