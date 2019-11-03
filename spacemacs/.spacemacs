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
     ;; main
     auto-completion
     git
     helm
     org
     ;; languages and file types
     javascript
     yaml
     sql
     vimscript
     html
     bibtex
     c-c++
     emacs-lisp
     haskell
     latex
     markdown
     python
     finance
     ;; extra
     spell-checking
     syntax-checking
     (shell :variables shell-default-height 30 shell-default-position 'bottom)
     chrome
     emoji
     evil-snipe
     fasd
     games
     gtags
     ranger
     ;; local
     (org_extra :location local)
     (lang :location local)
     (openwith :location local)
     (coq :location local)
     )
   dotspacemacs-additional-packages
   '(
     org-ref
     evil-ledger
     color-theme-sanityinc-solarized
     light-soap-theme
     (fira-code :location local)
     )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
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
   dotspacemacs-major-mode-leader-key "\\"
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
    (setq make-backup-files t)
    (setq backup-directory-alist `(("." . "~/.emacs.d/.cache/backup")))
    (setq version-control t    ; Use version numbers for backups
          delete-old-versions t  ; don't ask if can delet old versions
          kept-new-versions 3)   ; Number of newest versions to keep
    ;; save undo-tree
    (setq undo-tree-auto-save-history t)
	(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo")))
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
	;; bibtex paths
	(setq bibtex-completion-bibliography '("~/Dropbox/Ref/ref.bib")
          bibtex-completion-library-path '("~/Dropbox/Ref/pdfs/")
          bibtex-completion-notes-path "~/Dropbox/Ref/ref.org")
	;; bibtex search in tag field
	(setq bibtex-completion-additional-search-fields '(tags))

    ;; ------------------- finance ------------------------------
    (add-hook 'ledger-mode-hook #'evil-ledger-mode)

    ;; =================== Custom file for auto inserted customization =================================

    (setq custom-file (concat user-emacs-directory "private/custom.el"))
    (load custom-file)
  )
