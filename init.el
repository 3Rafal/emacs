(require 'package)

; List the packages you want
(setq package-list '(evil
		     evil-collection
		     org-bullets
		     csharp-mode
		     omnisharp
		     magit
		     evil-magit
		     helm
		     spacemacs-theme
		     projectile
		     treemacs
		     treemacs-evil
		     treemacs-projectile
		     company
		     elpy))

; Add Melpa as the default Emacs Package repository
; only contains a very limited number of packages
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

; Activate all the packages (in particular autoloads)
(package-initialize)

(load-theme 'spacemacs-light t)
; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; Evil mode
(setq evil-want-keybinding nil)
;; scroll with C-u
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode t)
(evil-collection-init)

;; Non-blinking cursor in evil
(blink-cursor-mode -1)

; Company
(add-hook 'after-init-hook 'global-company-mode)

;; helm setup
(require 'helm-config)
(require 'helm)
(helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)
(setq helm-display-header-line nil)

;; Use Org-Bullets in Org-mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; Projectile default recommended
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Electric-pair-mode in C#
(defun my-csharp-mode-hook ()
  (electric-pair-local-mode 1) ;; Emacs 25
  )
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

;; Use Omnisharp
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;; Magit global status keybinding
(global-set-key (kbd "C-x g") 'magit-status)
;; use Evil-Magit
(require 'evil-magit)

; Treemacs
(global-set-key (kbd "C-x t") 'treemacs)

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

; Run powershell
(defun run-powershell ()
  "Run powershell"
  (interactive)
  (async-shell-command "c:/windows/system32/WindowsPowerShell/v1.0/powershell.exe -Command -"
               nil
               nil))

; Enable show-paren-mode
(show-paren-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (projectile spacemacs-theme org-bullets omnisharp helm evil-magit evil-leader evil-collection))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
