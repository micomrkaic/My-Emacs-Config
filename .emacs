;; -*- lexical-binding: t; -*-

;; --------------------------
;; Package Setup
;; --------------------------
(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Only refresh archives if missing
(unless (file-exists-p (expand-file-name "archives/melpa/archive-contents" package-user-dir))
  (package-refresh-contents))

(package-initialize)

;; --------------------------
;; Bootstrap use-package
;; --------------------------
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; --------------------------
;; Projectile Setup
;; --------------------------
(require 'projectile)
(setq projectile-save-known-projects t)
(setq projectile-remember-projects-under-project-root t)
(projectile-mode +1)

;; Manually load saved projects early
(when (file-exists-p (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))
    (setq projectile-known-projects (read (current-buffer)))))

;; --------------------------
;; Splash Screen Setup
;; --------------------------
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; --------------------------
;; Dashboard Setup
;; --------------------------
(require 'dashboard)
(setq dashboard-startup-banner 'official)
(setq dashboard-projects-backend 'projectile)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (bookmarks . 5)))
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(dashboard-setup-startup-hook)

;; Refresh dashboard after full init
(add-hook 'emacs-startup-hook #'dashboard-refresh-buffer)

;; --------------------------
;; Font Settings
;; --------------------------
;;(set-face-attribute 'default nil :font "IBM Plex Mono Medium")
(set-face-attribute 'default nil :font "Victor Mono-12")

;; (set-face-attribute 'default nil
;;   :family "Fira Code"
;;   :weight 'medium
;;   :height 120)  ;; Adjust size (100 = 10pt)

(defun my-c-mode-comment-italics ()
  (set-face-attribute 'font-lock-comment-face nil
    :slant 'italic))

(add-hook 'c-mode-hook #'my-c-mode-comment-italics)
(add-hook 'c++-mode-hook #'my-c-mode-comment-italics)

;; Load ligature.el from local path
(add-to-list 'load-path "~/.emacs.d/ligature")
(require 'ligature)

;; Define ligatures for programming modes
(ligature-set-ligatures 'prog-mode
  '("www" "**" "***" "|||" "-->" "->" "<-" "=>" "!=" "==" "===" ">=" "<=" "&&" "||" "::" "++" "--"))

;; Enable ligatures globally
(global-ligature-mode t)


(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FFD700")
          ("FIXME"  . "#FF4500")
          ("HACK"   . "#FF69B4")
          ("REVIEW" . "#00CED1")
          ("NOTE"   . "#00FF00"))))

(global-set-key (kbd "C-c t n") #'hl-todo-next)
(global-set-key (kbd "C-c t p") #'hl-todo-previous)

(show-paren-mode 1)
(setq show-paren-style 'expression)


;; --------------------------
;; Compilation Settings
;; --------------------------
(defun my/find-makefile-dir (dir)
  "Recursively search upward from DIR for a Makefile or makefile.
Returns the directory path if found, or nil if not."
  (let ((parent (file-name-directory (directory-file-name dir))))
    (cond
     ((directory-files dir nil "\\`[Mm]akefile\\'") dir)
     ((or (not parent) (equal dir parent)) nil)
     (t (my/find-makefile-dir parent)))))

(defun my/compile-project ()
  "Search for Makefile up the directory tree and compile using make -k."
  (interactive)
  (let* ((start-dir (or (buffer-file-name) default-directory))
         (make-dir (my/find-makefile-dir (file-name-directory start-dir))))
    (if make-dir
        (let ((default-directory make-dir))
          (save-some-buffers t)
          (compile "make -k"))
      (message "No Makefile found in project tree."))))

(global-set-key (kbd "C-x c") #'my/compile-project)

;; --------------------------
;; UI and Editor Settings
;; --------------------------

(tool-bar-mode -1)

(line-number-mode 1)
(column-number-mode 1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(setq whitespace-display-mappings '((space-mark 32 [183] [46])))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 200)

(defun my-c-mode-column-indicator ()
  (setq display-fill-column-indicator-column 100)
  (display-fill-column-indicator-mode 1))
(add-hook 'c-mode-hook #'my-c-mode-column-indicator)
(add-hook 'c++-mode-hook #'my-c-mode-column-indicator)

;; --------------------------
;; Markdown Mode
;; --------------------------

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc"))

;; --------------------------
;; Custom
;; --------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tsdh-dark))
 '(package-selected-packages
   '(dashboard hl-todo markdown-mode projectile projectile-git-autofetch
	       rainbow-delimiters vterm))
 '(safe-local-variable-values
   '((flycheck-gcc-include-path "../include")
     (flycheck-clang-include-path "../include"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
