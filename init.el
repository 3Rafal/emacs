;; Disable menubar, toolbar and scrollbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

; Enable show-paren-mode
(show-paren-mode 1)

;; Minimalistic startup
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; Start in full-screen
;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Large threshold for init
(setq gc-cons-threshold (* 100 1000 1000))

(require 'package)

; Add Melpa as the default Emacs Package repository
; only contains a very limited number of packages
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org"   . "https://orgmode.org/elpa")
	("gnu"   . "http://elpa.gnu.org/packages/")))

; Activate all the packages (in particular autoloads)
(package-initialize)

; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; TODO: consider switching to defer by default
;; (setq use-package-always-defer t)

;; Package load debug
;;(setq use-package-verbose t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "9:00"))

(use-package no-littering)

(use-package lisp-mode
  :ensure nil
  :commands emacs-lisp-mode
  :bind (("C-c C-r" . 'eval-region)
	 ("C-c C-l" . 'eval-buffer)))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  (blink-cursor-mode -1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; make Esc quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Non-blinking cursor in evil
(global-set-key (kbd "M-u") 'universal-argument)

;; Org 
(defun rg/org-hyphen-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  :defer t
  :config
  (setq org-ellipsis " ▼"
	org-startup-indented t)
  (rg/org-hyphen-setup))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org-life/roam/")
  (org-roam-db-location "~/org-life/roam/db.sqlite")
  (org-roam-dailies-directory "daily/")
  (org-roam-file-completion-tag-position 'append)
  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n g" . org-roam-graph)
	       ("C-c n t" . org-roam-dailies-find-today)
	       ("C-c n m" . org-roam-dailies-find-tomorrow)
	       ("C-c n y" . org-roam-dailies-find-yesterday)
	       ("C-c n a" . org-roam-tag-add))
	 :map org-mode-map
	      (("C-c n i" . org-roam-insert))
	      (("C-c n I" . org-roam-insert-immediate)))
  :init
  (setq org-roam-db-update-method 'immediate))

(setq org-roam-capture-templates
      '(("d" "default" plain
         (function org-roam-capture--get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+title: ${title}\n\n\n"
         :unnarrowed t)))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :config
  (projectile-mode)
  :custom
  ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; .NET
;;;; Supress 'Package cl is deprecated' warning
(setq byte-compile-warnings '(cl-functions))
(use-package eglot-fsharp
  :defer t)

(use-package fsharp-mode
  :defer t
  :init
  (setq inferior-fsharp-program "dotnet fsi --readline-"
	fsharp-autosave-on-file-load t))

(use-package csproj-mode
  :defer t)

;; Haskell setup
(defun rg/haskell-evil-open-above ()
  (interactive)
  (evil-digit-argument-or-evil-beginning-of-line)
  (haskell-indentation-newline-and-indent)
  (evil-previous-line)
  (haskell-indentation-indent-line)
  (evil-append-line nil))

(defun rg/haskell-evil-open-below ()
  (interactive)
  (evil-append-line nil)
  (haskell-indentation-newline-and-indent))

(use-package haskell-mode
  :defer t
  :hook
  (haskell-mode . interactive-haskell-mode)
  :config
  (evil-collection-define-key 'normal 'haskell-mode-map
    "o" 'rg/haskell-evil-open-below
    "O" 'rg/haskell-evil-open-above))

(use-package lsp-mode
  :hook ((haskell-mode . lsp)
	 (fsharp-mode . lsp))
  :commands lsp
  :init
  (setq lsp-use-native-json t
	lsp-print-performance nil
	lsp-log-io nil
	lsp-diagnostics-modeline-scope :project
	lsp-file-watch-threshold 5000
	lsp-ui-doc-show-with-cursor nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-position (quote bottom))
  :config
  (setq company-minimum-prefix-length 1)
  (eldoc-mode -1))

(use-package lsp-haskell
  :after haskell-mode
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"
	lsp-haskell-process-wrapper-function (lambda (argv) (append '("nice") argv))
	lsp-haskell-process-args-hie nil)
  ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;(setq lsp-log-io t)
  ;; (define-key evil-normal-state-map "gd" 'intero-goto-definition)
  (define-key evil-normal-state-map "gn" 'flycheck-next-error)
  (define-key evil-normal-state-map "gp" 'flycheck-previous-error))

(use-package ormolu
  :after haskell-mode
 ; :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind
  (:map haskell-mode-map
	("C-c r" . ormolu-format-buffer)))

(use-package magit
  :defer t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-x g" . #'magit-status)))

(use-package treemacs
  :bind
  (:map global-map
	("C-x t" . treemacs)))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :defer t
  :after treemacs projectile)

; Sort apropos by relevancy
(setq apropos-sort-by-scores t)


(set-face-attribute 'default nil :font "Fira Code" :height 100)

; don't create backup and autosave files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

; ido
(ido-mode 1)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
	 ;; ("C-c C-r" . counsel-rg)
	 ("C-c C-p" . counsel-projectile-rg)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; doom modeline
(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package helpful
  :commands (helpful-callable
	     helpful-variable
	     helpful-command
	     helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package company
  :defer t
  :config
  (global-company-mode)
  (progn
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

; dired
(use-package dired
  :defer t
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

; terms/shells

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
        vterm-max-scrollback 10000))

(defun rg/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :defer t ;; TODO: eshell is still loaded on init...
  :hook (eshell-first-time-mode . rg/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t
          eshell-visual-commands '("htop"))))

(use-package eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

(use-package geiser
  :defer t)

(use-package hydra
  :defer t)

(defhydra hydra-zoom (global-map "C-c h z")
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset")
  ("e" (text-scale-set 0) nil :bind nil :exit t))

(use-package elpy
  :defer t
  :init
  (elpy-enable))

;; Does it work?
(add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8)))
(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
(add-to-list 'process-coding-system-alist '("flake8" . (utf-8 . utf-8)))

;; Measure performance
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Bring back to small threshold after init.
(setq gc-cons-threshold (* 5 1000 1000))
