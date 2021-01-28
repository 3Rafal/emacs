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

(require 'package)

; List the packages you want
(setq package-list '(org-bullets
		     haskell-mode))

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

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
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

;; Use Org-Bullets in Org-mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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

;; Haskell setup
;;; Haskell interactive
(require 'haskell-interactive-mode)
(require 'haskell-indent)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; disable eldoc in haskell-mode
(add-hook 'haskell-mode-hook #'(lambda () (eldoc-mode -1)))


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

; Enable show-paren-mode
(show-paren-mode 1)

(set-face-attribute 'default nil :font "Fira Code" :height 100)

; don't create backup and autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
