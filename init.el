
;; Disable menubar, toolbar and scrollbar
(menu-bar-mode -1)
(when (display-graphic-p)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))

; Enable show-paren-mode
(show-paren-mode 1)
(blink-cursor-mode 0)

;; Minimalistic startup
(setq inhibit-startup-screen t
      initial-scratch-message nil)
(setq column-number-mode t)

;; Use mouse in tty
(add-hook 'tty-setup-hook #'xterm-mouse-mode)
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (setq frame-resize-pixelwise t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)


  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-l --group-directories-first"))

(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;(set-frame-parameter nil 'undecorated t)

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

;; Package load debug
;;(setq use-package-verbose t)

(setq comint-input-ignoredups t)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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

(use-package direnv
 :config
 (direnv-mode))

(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(use-package lisp-mode
  :ensure nil
  :commands emacs-lisp-mode
  :bind (("C-c C-r" . 'eval-region)
	 ("C-c C-l" . 'eval-buffer)))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(typescript-mode racket-mode opam-switch-mode web-mode nix-mode utop dune ocp-indent tuareg yaml-mode geiser eshell-git-prompt vterm company doom-themes helpful which-key rainbow-delimiters all-the-icons ivy-rich magit lsp-mode counsel-projectile ace-window direnv no-littering auto-package-update exec-path-from-shell))
 '(warning-suppress-log-types '((comp) (comp) (comp) (comp) (comp) (comp)))
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp))))

(use-package magit
  :defer t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-x g" . #'magit-status)))

; Sort apropos by relevancy
(setq apropos-sort-by-scores t)

(set-face-attribute 'default nil :family "Mononoki" :height 170)

; don't create backup and autosave files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-x b" . counsel-ibuffer)
         ("C-c a" . counsel-ag)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config (counsel-mode 1))

;; doom modeline
(use-package all-the-icons)
;(use-package doom-modeline
;  :init (doom-modeline-mode 1))

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
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  :bind (("C-x C-j" . dired-jump)))

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

(defun sh ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " (generate-new-buffer-name "*shell*"))))
      (shell shell-name)))

(use-package geiser
  :defer t)

(use-package yaml-mode)

(add-hook 'c-mode-hook #'electric-pair-mode)

;; ocaml
;;; Major mode for editing OCaml files.
(use-package tuareg :ensure)

;; Consistent indentation.
(use-package ocp-indent :ensure
  :after tuareg
  :hook
  (tuareg-mode . ocp-setup-indent))

;; Pyret
(load "~/.emacs.d/pyret/pyret.el")
(load "~/.emacs.d/pyret/pyret-debug-mode.el")

(use-package typescript-mode)

;; JavaScript
(setq js-indent-level 2)

(defun wslp ()
  "Return t if running in WSL."
  (and (eq system-type 'gnu/linux)
       (string-match "microsoft" (shell-command-to-string "uname -r"))))

(when (wslp)
  ;; Fix WSLg clipboard behavior
  (setq select-active-regions nil
        select-enable-clipboard t
        select-enable-primary nil
        interprogram-cut-function #'gui-select-text))

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
(put 'narrow-to-region 'disabled nil)
