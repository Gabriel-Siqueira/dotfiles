
(setq user-full-name "Gabriel Siqueira"
      user-mail-address "gabriel.gabrielhs@gmail.com"
      calendar-latitude -22.9
      calendar-longitude -47.1
      calendar-location-name "Campinas, Sao Paulo, Brasil")

(require 'package)
(push '("melpa-stable" . "http://stable.melpa.org/packages/")
                        package-archives)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
                        package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
                        package-archives)
(push '("org" . "http://orgmode.org/elpa/")
                        package-archives)
(push '("gnu" . "http://elpa.gnu.org/packages/")
                        package-archives)
(package-initialize)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; update packages
(when (not package-archive-contents) (package-refresh-contents))

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(defun install-all () (interactive)
       (or
        (require-package 'ace-jump-mode)
        (require-package 'auto-complete)
        (require-package 'cider)
        (require-package 'clojure-mode)
        (require-package 'ediprolog)
        (require-package 'evil)
        (require-package 'evil-leader)
        (require-package 'evil-nerd-commenter)
        (require-package 'evil-numbers)
        ;; (require-package 'evil-smartparens)
        (require-package 'evil-surround)
        (require-package 'evil-tabs)
        (require-package 'fill-column-indicator)
        (require-package 'flymake)
        (require-package 'flymake-cursor)
        (require-package 'ghc)
        (require-package 'folding)
        (require-package 'haskell-mode)
        (require-package 'helm)
        (require-package 'indent-guide)
        (require-package 'linum-relative)
        (require-package 'lua-mode)
        (require-package 'multiple-cursors)
        (require-package 'multi-term)
        (require-package 'neotree)
        (require-package 'org-bullets)
        ;; (require-package 'pallet)
        ;; (require-package 'paxedit)
        (require-package 'php-mode)
        (require-package 'prolog)
        (require-package 'rainbow-delimiters)
        (require-package 'smart-mode-line)
        ;; (require-package 'smartparens)
        (require-package 'smex)
        (require-package 'use-package)
        (require-package 'yasnippet)
        ))

(blink-cursor-mode -1)          ; cursor does not blink
(tool-bar-mode -1)              ; no tool bar
(set 'menu-bar-mode nil)        ; remove menu bar
(display-time-mode t)           ; display time
(set 'inhibit-startup-screen t) ; no startup screen

(custom-set-faces
 `(default ((t (:family "Terminus" :foundry "xos4" :slant normal :weight bold :height 105 :width normal))))
 `(buffer-menu-buffer ((t (:weight bold)))))

;; how ANSI escape sequences will be translate into faces
(set 'ansi-color-faces-vector 
[default default default italic underline success warning error])

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Theme
(if (file-exists-p "~/.emacs.d/themes/misterioso-blue-theme.el") 
              (load-theme 'misterioso-blue) 
              (load-theme 'misterioso))

(use-package smart-mode-line
   :ensure t
   :config (sml/setup))

(use-package anzu 
         :ensure t
         :init (global-anzu-mode +1))

(use-package pretty-mode
   :ensure t
   :config (global-pretty-mode t))

;; Start emacs server
      (server-start)

;; Indentation 4 spaces
(setq-default tab-width 4)
(setq c-basic-offset 4)

(menu-bar-mode 0)             ; remove menu bar ok
(show-paren-mode 1)           ; match parents, breckets, etc
(setq-default fill-column 77) ; line size
(setq visible-bell 1)         ; no beep

;; yes is y and no is n
(fset 'yes-or-no-p 'y-or-n-p)

;; Isolate kill ring
(setq interprogram-cut-function nil)
(setq interprogram-paste-function nil)

;; Directory to save beckup files
(setq backup-directory-alist `(("." . "~/Documents/swap_files")))
(setq version-control t    ; Use version numbers for backups.
      kept-new-versions 8  ; Number of newest versions to keep.
      kept-old-versions 2) ; Number of oldest versions to keep.
  
;; different color for terminal emacs
(if (window-system) nil (load-theme 'tango-dark) )
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(use-package evil 
 :ensure t
  :config
                              (setq evil-shift-width 4)
      (evil-mode 1)
)

;; require for evil folding
(add-hook 'prog-mode-hook 'hs-minor-mode)

(setq evil-emacs-state-cursor '("orange" box))
(setq evil-normal-state-cursor '("red" box))
(setq evil-visual-state-cursor '("yellow" box))
(setq evil-insert-state-cursor '("green" bar))
(setq evil-replace-state-cursor '("grey" box))
(setq evil-operator-state-cursor '("red" hollow))

(use-package evil-numbers
   :ensure t
      )

(use-package evil-surround
   :ensure t
   :config (global-evil-surround-mode 1))

(use-package evil-nerd-commenter 
   :ensure t
   :config (evilnc-default-hotkeys))

(use-package evil 
    :config
                        (add-hook 'neotree-mode-hook
                                                                 (lambda ()
                                                                         (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                                                                         (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
                                                                         (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                                                                         (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))
)

(use-package org
   :ensure t)

      ;; hooks for minor modes
      (add-hook 'org-mode-hook 'flyspell-mode)
      (add-hook 'org-mode-hook 'auto-complete-mode)

      ;; Source code block
      (setq org-src-fontify-natively t)
      (setq org-src-tab-acts-natively t)

;; Replace ... with ⤵
(setq org-ellipsis "⤵")

;; Replace * for bullets
(use-package org-bullets
   :ensure t
   :config
                              (add-hook 'org-mode-hook
                                                                      (lambda ()
                                                                              (org-bullets-mode t)))
                              (setq org-hide-leading-stars t))

;; Size of mathematical images
(setq org-format-latex-options 
(plist-put org-format-latex-options :scale 1.5))

(use-package clojure-mode
   :ensure t)

(use-package haskell-mode
   :ensure t
   :config
                              (add-hook 'haskell-mode-hook
                                                                      (lambda ()
                                                                              (haskell-doc-mode)
                                                                              (turn-on-haskell-indent)
                                                                              (ghc-init))))

(use-package lua-mode
   :ensure t)

(use-package php-mode
   :ensure t)

(use-package prolog
   :ensure t
   :config
                              ;; Mode for prolog
                              (setq prolog-system 'swi)
                              (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                                                                                                                                      ("\\.m$" . mercury-mode))
                                                                                                                                               auto-mode-alist)))

;; ediprolog
(use-package ediprolog
   :ensure t)

;; flymake for prolog
(add-hook 'prolog-mode-hook
        (lambda ()
          (require 'flymake)
          (make-local-variable 'flymake-allowed-file-name-masks)
          (make-local-variable 'flymake-err-line-patterns)
          (setq flymake-err-line-patterns
                '(("ERROR: (?\\(.*?\\):\\([0-9]+\\)" 1 2)
                  ("Warning: (\\(.*\\):\\([0-9]+\\)" 1 2)))
          (setq flymake-allowed-file-name-masks
                '(("\\.pl\\'" flymake-prolog-init)))
          (flymake-mode 1)))

(defun flymake-prolog-init ()
(let* ((temp-file   (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
       (local-file  (file-relative-name
                     temp-file
                     (file-name-directory buffer-file-name))))
  (list "swipl" (list "-q" "-t" "halt" "-s " local-file))))

(use-package company
   :ensure t
   :config
                              (add-hook 'after-init-hook 'global-company-mode)
      )

(use-package yasnippet 
   :ensure t
   :config
                              (setq yas-snippet-dirs '("~/.emacs.d/mysnippets"))
                              (yas-reload-all)
                              (add-hook 'prog-mode-hook 'yas-minor-mode)
                              (add-hook 'ess-mode-hook 'yas-minor-mode)
                              (add-hook 'markdown-mode-hook 'yas-minor-mode)
                              (add-hook 'org-mode-hook 'yas-minor-mode))

(fset 'my-complete-file-name
        (make-hippie-expand-function '(try-complete-file-name-partially
                                       try-complete-file-name)))

(setq electric-pair-mode t)

(use-package rainbow-delimiters
   :ensure t
   :config
                              (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
                              (add-hook 'org-mode-hook #'rainbow-delimiters-mode))

(use-package multi-term
   :ensure t
   :config 
                                    (setq multi-term-program "/bin/zsh"))

(use-package helm
   :ensure t
   :config (helm-mode 1))

(use-package smex 
   :ensure t
               :config
                              (smex-initialize))

(use-package eyebrowse
         :ensure t
         :init (eyebrowse-mode t)
         :config (eyebrowse-setup-opinionated-keys)
)

(use-package neotree 
   :ensure t)

(when (fboundp 'winner-mode)
    (winner-mode 1))

(use-package ace-jump-mode
   :ensure t)

(use-package fill-column-indicator
   :ensure t
               :config
                              (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
                              (global-fci-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
                      (set-face-attribute
                              'flycheck-error nil :foreground "red" :underline "red")
                      (set-face-attribute
                              'flycheck-warning nil :foreground "yellow" :underline "yellow")
                      (set-face-attribute
                              'flycheck-info nil :foreground "blue" :underline "blue")
)

(use-package folding
   :ensure t
               :config
                              (load "folding" 'nomessage 'noerror)
                              (folding-mode-add-find-file-hook)

                              ;; new marks
                              (folding-add-to-marks-list 'emacs-lisp-mode ";;{{{" ";;}}}" nil t)
                              (folding-add-to-marks-list 'lua-mode "-- {{{" "-- }}}" nil t)
                              (folding-add-to-marks-list 'haskell-mode "--{{{" "--}}}" nil t)

                              (add-hook 'prog-mode-hook (lambda() (folding-mode)))
                              (let* ((ptr (assq 'asm-mode folding-mode-marks-alist)))
                                                                               (setcdr ptr (list "@*" "@-")))
              )

(use-package hl-line
   :ensure t
               :config
                              (global-hl-line-mode)
                              (set-face-background hl-line-face "black"))

(use-package indent-guide
   :ensure t
               :config
                              (indent-guide-global-mode))

(if (and (file-exists-p "~/.offlineimaprc") (file-exists-p "~/Documents/Maildir"))
        (use-package mu4e

        :config
                ;; default
                (setq mu4e-maildir (expand-file-name "~/Documents/Maildir"))

                (setq mu4e-drafts-folder "/[Gmail].Drafts")
                (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
                (setq mu4e-trash-folder  "/[Gmail].Trash")

                ;; don't save message to Sent Messages, GMail/IMAP will take care of this
                (setq mu4e-sent-messages-behavior 'delete)

                ;; setup some handy shortcuts
                (setq mu4e-maildir-shortcuts
                                        '(("/INBOX"             . ?i)
                                                ("/[Gmail].Sent Mail" . ?s)
                                                ("/[Gmail].Trash"     . ?t)))

                ;; allow for updating mail using 'U' in the main view:
                (setq mu4e-get-mail-command "offlineimap")

                ;; Personal info
                (setq
                 user-mail-address "gabriel.gabrielhs@gmail.com"
                 user-full-name  "Gabriel Henriques siqueira"
                 ;; message-signature ""
                )

                ;; sending mail -- replace USERNAME with your gmail username
                ;; also, make sure the gnutls command line utils are installed
                ;; package 'gnutls-bin' in Debian/Ubuntu, 'gnutls' in Archlinux.

                (use-package smtpmail
                        :config
                        (setq message-send-mail-function 'smtpmail-send-it
                                                smtpmail-stream-type 'starttls
                                                smtpmail-default-smtp-server "smtp.gmail.com"
                                                smtpmail-smtp-server "smtp.gmail.com"
                                                smtpmail-smtp-service 587)
                )
        ) nil )

(use-package multiple-cursors
   :ensure t)

(use-package origami
   :ensure t
   :config
                         (global-origami-mode)
)

(load "~/.emacs.d/lisp/PG/generic/proof-site")

(use-package linum-relative
   :ensure t
               :config
                              (linum-relative-on)
                              (global-linum-mode))

(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

(use-package which-key
   :ensure t
               :config
                              (which-key-mode)
                              (setq which-key-show-operator-state-maps t)
)

(defun my/change-dictionary ()
  " change dictionary."
              (interactive)
              (print
               (if (string= ispell-dictionary "english")
                               (setq ispell-dictionary "pt_BR")
                               (if (string= ispell-dictionary "pt_BR")
                                               (setq ispell-dictionary "de_DE")
                                               (setq ispell-dictionary "english")))))

(defun my/move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun my/move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (my/move-line (if (null n) -1 (- n))))

(defun my/move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (my/move-line (if (null n) 1 n)))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
;; key bindings
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(defun my/toggle-clip ()
  "Enable or Disable system clipboard."
  (interactive)
      (if (not interprogram-cut-function)
          (and
           (setq interprogram-cut-function 'x-select-text)
           (setq interprogram-paste-function 'x-selection-value))
          (or
           (setq interprogram-cut-function nil)
           (setq interprogram-paste-function nil))
))

(defun my/file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(use-package evil-leader
   :ensure t
   :config
                      (global-evil-leader-mode)
                      (evil-leader/set-leader "ç"))

(evil-leader/set-key "c" 'my/toggle-clip)

;; prolog
(add-hook 'prolog-mode-hook  (evil-leader/set-key "e" 'ediprolog-dwim))

(evil-leader/set-key "n+" 'evil-numbers/inc-at-pt)
(evil-leader/set-key "n-" 'evil-numbers/dec-at-pt)

(evil-leader/set-key "f" 'folding-toggle-show-hide) ; key bindin

(evil-leader/set-key "gb" 'switch-to-buffer)   ; change buffer
(evil-leader/set-key "gw" 'ace-jump-word-mode) ; çw for Ace Jump (word)
(evil-leader/set-key "gl" 'ace-jump-line-mode) ; çl for Ace Jump (line)
(evil-leader/set-key "gc" 'ace-jump-char-mode) ; çc for Ace Jump (char)
(evil-leader/set-key "gs" 'other-window)       ; go to another split
;;  go to previous heading (org) 
(evil-leader/set-key "gp" 'outline-previous-visible-heading)
;; go to next heading (org)
(evil-leader/set-key "gn" 'outline-next-visible-heading)

(global-set-key [f6] 'menu-bar-mode)

(evil-leader/set-key "/" 'my-complete-file-name)

(evil-leader/set-key ":e" 'eval-expression)
(evil-leader/set-key ":s" 'smex-major-mode-commands)
(evil-leader/set-key ":m" 'smex-major-mode-commands)
(evil-leader/set-key ":h" 'helm-M-x)

(evil-leader/set-key "<up>" 'my/move-line-up)
(evil-leader/set-key "k" 'my/move-line-up)
(evil-leader/set-key "<down>" 'my/move-line-down)
(evil-leader/set-key "j" 'my/move-line-down)

(evil-leader/set-key "mm" 'mc/edit-lines)
(evil-leader/set-key "m>" 'mc/mark-next-like-this)
(evil-leader/set-key "m<" 'mc/mark-previous-like-this)
(evil-leader/set-key "ma" 'mc/mark-all-like-this)

(evil-leader/set-key "[" 'origami-open-node)
(evil-leader/set-key "]" 'origami-close-node)
(evil-leader/set-key "{" 'origami-open-node-recursively)
(evil-leader/set-key "}" 'origami-close-node-recursively)
(evil-leader/set-key "ª" 'origami-open-all-nodes)
(evil-leader/set-key "º" 'origami-close-all-nodes)

(global-set-key [f7] 'flycheck-list-errors)

(evil-leader/set-key "ss" 'flyspell-mode)
(evil-leader/set-key "sc" 'my/change-dictionary)

(global-set-key [f5] 'neotree-toggle)

(evil-leader/set-key "C-l" 'org-preview-latex-fragment)

(eval-after-load "paxedit"
  '(progn (define-key paxedit-mode-map (kbd "M-<right>") 'paxedit-transpose-forward)
          (define-key paxedit-mode-map (kbd "M-<left>") 'paxedit-transpose-backward)
          (define-key paxedit-mode-map (kbd "M-<up>") 'paxedit-backward-up)
          (define-key paxedit-mode-map (kbd "M-<down>") 'paxedit-backward-end)
          (define-key paxedit-mode-map (kbd "M-b") 'paxedit-previous-symbol)
          (define-key paxedit-mode-map (kbd "M-f") 'paxedit-next-symbol)
          (define-key paxedit-mode-map (kbd "C-%") 'paxedit-copy)
          (define-key paxedit-mode-map (kbd "C-&") 'paxedit-kill)
          (define-key paxedit-mode-map (kbd "C-*") 'paxedit-delete)
          (define-key paxedit-mode-map (kbd "C-~") 'paxedit-sexp-raise)
          ;; Symbol backward/forward kill
          (define-key paxedit-mode-map (kbd "M-k M-b") 'paxedit-backward-kill)
          (define-key paxedit-mode-map (kbd "M-k M-f") 'paxedit-forward-kill)
          ;; Symbol manipulation
          (define-key paxedit-mode-map (kbd "M-u") 'paxedit-symbol-change-case)
          (define-key paxedit-mode-map (kbd "M-l") 'paxedit-symbol-copy)
          (define-key paxedit-mode-map (kbd "M-k M-k") 'paxedit-symbol-kill)))

;; relative/absolute lines
(evil-leader/set-key "l" 'linum-relative-toggle)

(global-set-key [f8] 'multi-term-dedicated-toggle)
(add-hook 'term-mode-hook
          (lambda ()
            (evil-leader/set-key "tl" 'term-line-mode)
            (evil-leader/set-key "tc" 'term-char-mode)))

(evil-leader/set-key "=" 'count-words-region)

(evil-leader/set-key "hs" 'hs-show-block) 
(evil-leader/set-key "hS" 'hs-show-all) 
(evil-leader/set-key "hh" 'hs-hide-block) 
(evil-leader/set-key "hH" 'hs-hide-all)

(evil-leader/set-key "\"y" 'copy-to-register)
(evil-leader/set-key "\"p" 'insert-register)

(evil-leader/set-key "w" 'visual-line-mode)

(evil-leader/set-key "+" 'text-scale-increase)
(evil-leader/set-key "-" 'text-scale-decrease)

;; read local file if exists
(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))
