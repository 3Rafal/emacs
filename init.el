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
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; Org-mode set width
(setq-default fill-column 80)

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  :custom
  ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; .NET
(use-package fsharp-mode
  :defer t
  :init
  (setq inferior-fsharp-program "dotnet fsi --readline-"
	fsharp-autosave-on-file-load t))

(use-package csproj-mode
  :defer t)

;; Haskell setup
(use-package haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode))

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
 ; :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
   ("C-c r" . ormolu-format-buffer)))

(use-package magit
  :ensure t
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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; doom modeline
(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.5))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package helpful
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
  :init (global-company-mode)
  :config
  (progn
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

; terms/shells

(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
        vterm-max-scrollback 10000))

(use-package eshell-git-prompt)
(use-package eshell
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

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

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . rg/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t
          eshell-visual-commands '("htop")))

  (eshell-git-prompt-use-theme 'robbyrussell))

(use-package geiser)

(use-package hydra)

(defhydra hydra-zoom (global-map "C-c h z")
  "zoom"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset")
  ("e" (text-scale-set 0) nil :bind nil :exit t))

