(require 'package)

; List the packages you want
(setq package-list '(evil
		     evil-collection
		     org-bullets
		     omnisharp
		     magit
		     fsharp-mode
		     projectile
		     paredit
		     geiser
		     company
		     elpy
		     haskell-mode
		     csproj-mode
		     omnisharp
		     counsel))

; Add Melpa as the default Emacs Package repository
; only contains a very limited number of packages
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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

; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Evil mode
(setq evil-want-keybinding nil)
;; make Esc quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; scroll with C-u
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode t)
(evil-collection-init)

;; Non-blinking cursor in evil
(blink-cursor-mode -1)

(global-set-key (kbd "M-u") 'universal-argument)

; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Use Org-Bullets in Org-mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Org-mode set width
(setq-default fill-column 80)

; Projectile default recommended
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Haskell setup
;;; Haskell interactive
(require 'haskell-interactive-mode)
(require 'haskell-indent)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; disable eldoc in haskell-mode
(add-hook 'haskell-mode-hook #'(lambda () (eldoc-mode -1)))

;; F#
(require 'fsharp-mode)
(require 'eglot-fsharp)
(require 'csproj-mode)
(setq inferior-fsharp-program "dotnet fsi")

;; C#
(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  (electric-pair-local-mode 1)

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(doom-themes which-key rainbow-delimiters doom-modeline use-package fsharp-mode projectile spacemacs-theme org-bullets omnisharp helm evil-leader evil-collection)))

;; Magit global status keybinding
(global-set-key (kbd "C-x g") 'magit-status)

; Treemacs
(use-package treemacs
    :bind
    (:map global-map
	    ("C-x t" . treemacs)))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)


; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

; Enable elpy
(elpy-enable)

;; Disable menubar, toolbar and scrollbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Hide startup screen
(setq inhibit-startup-screen t)

;; Start in full-screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Show line numbers
(global-linum-mode t)

; Sort apropos by relevancy
(setq apropos-sort-by-scores t)

; Enable show-paren-mode
(show-paren-mode 1)

(set-face-attribute 'default nil :font "Fira Code Regular" :height 100)

; don't create backup and autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

; ido
(ido-mode 1)

;; Ivy config
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

  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;(doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;;(doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
