;; Disable menubar, toolbar and scrollbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

; Enable show-paren-mode
(show-paren-mode 1)

;; Minimalistic startup
(setq inhibit-startup-screen t
      initial-scratch-message nil)
(setq column-number-mode t)

;; Start in full-screen
;(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (setq frame-resize-pixelwise t))
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/libgccjit/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin20/10.2.0")
; try not to use tab characters ever when formatting code
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

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

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(setq comint-input-ignoredups t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 21)
  (auto-package-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "9:00"))

(use-package no-littering)
(setq-default tab-width 4)

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
  (setq-default evil-symbol-word-search nil)
  :config
  (evil-mode 1)
  (blink-cursor-mode -1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-u") 'universal-argument)

;; TRAMP
(setq tramp-default-method "ssh")
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local projectile-mode-line "Projectile"))))
(use-package docker-tramp)
(add-to-list 'directory-abbrev-alist
             '("^/revm" . "/ssh:user@10.244.1.1:/"))
(add-to-list 'directory-abbrev-alist
             '("^/srevm" . "/ssh:user@10.244.1.1|sudo::/"))
(add-to-list 'directory-abbrev-alist
             '("^/redo" . "/ssh:user@10.244.1.1|docker:user@devcontainer_dev_1:"))
(add-to-list 'directory-abbrev-alist
             '("^/rero" . "/ssh:user@10.244.1.1|docker:root@devcontainer_nginx_1:"))

(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:re:")
                   "direct-async-process" t
                   "remote-shell" "/bin/zsh"))
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))

(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(setq tramp-verbose 1)

(defun go-local ()
  "Shortcut for destroy all TRAMP connections and kill all associated buffers."
  (interactive)
  (ignore-errors (tramp-cleanup-all-connections))
  (ignore-errors (tramp-cleanup-all-buffers)))

(setq company-idle-delay 0.5
      company-minimum-prefix-length 2
      company-show-numbers t
      )
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

(use-package direnv)
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
  (haskell-mode . yas-minor-mode)
  :config
  (evil-collection-define-key 'normal 'haskell-mode-map
    "o" 'rg/haskell-evil-open-below
    "O" 'rg/haskell-evil-open-above))

(use-package yasnippet)
(use-package haskell-snippets)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(haskell-process-type 'cabal-new-repl)
 '(package-selected-packages
   '(docker-tramp direnv nix-mode merlin psc-ide purescript-mode protobuf-mode go-flycheck go-flymake go-mode sml-mode evil-surround csv-mode c-mode tuareg haskell-snippets yaml-mode idris-mode edwina rustic which-key vterm use-package treemacs-projectile treemacs-evil rainbow-delimiters proof-general ormolu org-roam org-bullets ob-fsharp no-littering magit lsp-ui lsp-haskell lispy ivy-rich helpful geiser evil-org evil-collection eshell-git-prompt elpy eglot-fsharp doom-themes doom-modeline dired-single dired-hide-dotfiles dash-functional csproj-mode csharp-mode counsel-projectile buttercup auto-package-update all-the-icons-dired)))

(use-package lsp-mode
  :hook (haskell-mode . lsp)
  :commands lsp
  :init
  (setq lsp-use-native-json t
	lsp-print-performance nil
	lsp-log-io nil
	lsp-diagnostics-modeline-scope :project
	lsp-file-watch-threshold 5000
	lsp-ui-doc-show-with-cursor nil)
  ;;:config
  ;; This is to make `lsp-mode' work with `direnv' and pick up the correct
  ;; version of GHC.
  (advice-add 'lsp :before #'direnv-update-environment)
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-position 'bottom)
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

; Sort apropos by relevancy
(setq apropos-sort-by-scores t)


(set-face-attribute 'default nil :family "mononoki")

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
         ("C-c a" . counsel-ag)
	     ;; ("C-c C-r" . counsel-rg)
	     ;; ("C-c C-p" . counsel-projectile-rg)
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
  :diminish company-mode
  :bind (:map company-active-map
			  ("<return>" . nil)
			  ("RET" . nil)
              ("<tab>" . company-complete-selection)))
(setq tab-always-indent 'complete)

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

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

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

(use-package idris-mode)
(setq idris-interpreter-path "~/.cabal/bin/idris")

;; cpp
(defun rg/compile-and-run ()
  (interactive)
  (let* ((src (file-name-nondirectory (buffer-file-name)))
         (exe (file-name-sans-extension src)))
    (compile (concat "g++ " src " -o " exe " && ./" exe))))

(use-package yaml-mode)

(add-hook 'c-mode-hook #'electric-pair-mode)

;; ocaml
(use-package tuareg
  :hook (tuareg-mode . merlin-mode)
  :hook (caml-mode . merlin-mode)
  )
(use-package merlin
  :bind (:map merlin-mode-map
			  ("C-c C-l" . tuareg-eval-buffer)
			  ("C-c C-r" . tuareg-eval-region)
			  )
  )
;;(add-hook 'caml-mode-hook #'merlin-mode)

;; sml
(use-package sml-mode)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package psc-ide)
(use-package purescript-mode)
(add-hook 'purescript-mode-hook
  (lambda ()
    (psc-ide-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-purescript-indentation)))

(use-package nix-mode
  :mode "\\.nix\\'")

;; Bring back to small threshold after init.
(setq gc-cons-threshold (* 5 1000 1000))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-quoted ((t (:inherit default)))))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
