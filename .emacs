;; -*- lexical-binding: t; -*-

;; --------------------------
;; Custom File Location
;; --------------------------
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; --------------------------
;; Package Repository Setup
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
;; UNDO tree
;; --------------------------
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;; --------------------------
;; INPUT method -- enable European characters
;; --------------------------
(set-input-method "latin-2-prefix")

;; --------------------------
;; ORG Mode Setup
;; --------------------------
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  ;; Vector of strings per level:
  (org-modern-star ["●" "◉" "○" "✦" "•" "‣"])
  ;; Hide the original stars:
  (org-modern-hide-stars t)
  ;; Nicer ellipsis when a heading is folded:
  (org-modern-ellipsis " … ")
)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(setq org-hide-emphasis-markers t)  ;; hides *bold* /italic/ markers
(setq org-pretty-entities t)        ;; renders symbols (like →, α, β, etc.)
(setq org-startup-indented t)       ;; nice visual indentation

;; --------------------------
;; Projectile Setup
;; --------------------------
(use-package projectile
  :ensure t
  :init
  (setq projectile-save-known-projects t
        projectile-remember-projects-under-project-root t)
  :config
  (projectile-mode +1)
  ;; Manually load saved projects early
  (let ((bmk (expand-file-name "projectile-bookmarks.eld" user-emacs-directory)))
    (when (file-exists-p bmk)
      (with-temp-buffer
        (insert-file-contents bmk)
        (setq projectile-known-projects (read (current-buffer)))))))

;; --------------------------
;; Splash Screen Setup
;; --------------------------
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; --------------------------
;; Dashboard Setup
;; --------------------------
(use-package dashboard
  :ensure t
  :after projectile
  :init
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        dashboard-startup-banner "~/.emacs.d/morpheus-small.png"
        dashboard-projects-backend 'projectile
        dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5))
        dashboard-center-content t
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil)
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'emacs-startup-hook #'dashboard-refresh-buffer))

;; --------------------------
;; Font and Ligature Settings
;; --------------------------
(set-face-attribute 'default nil
  :family "Victor Mono"
  :height 110)  ;; adjust to your preferred size (100 = 10pt, 110 = 11pt, etc.)

(defun my-c-mode-comment-italics ()
  (set-face-attribute 'font-lock-comment-face nil
    :slant 'italic))

(add-hook 'c-mode-hook #'my-c-mode-comment-italics)
(add-hook 'c++-mode-hook #'my-c-mode-comment-italics)

(defun my-elisp-comment-italics ()
  (set-face-attribute 'font-lock-comment-face nil
    :slant 'italic))

(add-hook 'emacs-lisp-mode-hook #'my-elisp-comment-italics)

;; Load ligature.el 
(use-package ligature
  :ensure t
  :commands (ligature-mode)
  ;; :config  ;; put your ligature-set-ligatures here if you like
)

;; Define ligatures for programming modes
;; (ligature-set-ligatures 'prog-mode
;;   '("www" "**" "***" "|||" "-->" "->" "<-" "=>" "!=" "==" "===" ">=" "<=" "&&" "||" "::" "++" "--"))

;; ;; Enable ligatures globally
;; (global-ligature-mode t)

;; Define ligatures for markdown and LaTeX modes
(dolist (mode '(markdown-mode LaTeX-mode))
  (ligature-set-ligatures mode
    '("www" "**" "***" "|||" "-->" "->" "<-" "=>" "!=" "==" "===" ">=" "<=" "&&" "||" "::" "++" "--"))
  (add-hook (intern (concat (symbol-name mode) "-hook")) #'ligature-mode))

;; --------------------------
;; hl-todo Package Settings
;; --------------------------
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
;; IDO Package Settings
;; --------------------------
;; (ido-mode 1)
;; (setq ido-enable-flex-matching t) ;; allows partial matches
;; (setq ido-everywhere t)

;; --------------------------
;; Vertico + Orderless + Marginalia (IDO Replacement)
;; --------------------------

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))) ;; for better file matching
  )

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))


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
(menu-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
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
;; Line Numbering
;; --------------------------
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; --------------------------
;; Olivetti Mode Fine Tuning
;; --------------------------
(use-package olivetti :ensure t :commands olivetti-mode)
(use-package wc-mode  :ensure t :commands wc-mode)

;; Save font and line-number state
(defvar-local my/olivetti-font-override nil)

(defun my/olivetti-mode-setup ()
  "Adjust font and line numbers for Olivetti mode."
  (if olivetti-mode
      (progn
        ;; Disable line numbers
        (display-line-numbers-mode 0)
	;; Turn on wc-mode
	(wc-mode 1)
	;; Underline mispelled words
	(flyspell-mode 1)
	;; Break long lines nicely
	(visual-line-mode 1)
        ;; Change font to Charis SIL
        (setq my/olivetti-font-override
              (face-remap-add-relative 'default :family "Charis SIL" :height 140)))
    ;; When Olivetti mode is turned off undo the special settings
    (display-line-numbers-mode 1)
    (wc-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -11)

    (setq display-line-numbers-type 'relative)
    ;; Restore font to the default font family
    (when my/olivetti-font-override
      (face-remap-remove-relative my/olivetti-font-override)
      (setq my/olivetti-font-override nil))))

;; Save per-buffer original menu bar state
(defvar-local my/olivetti-menu-bar-was-enabled nil)

(defun my/olivetti-toggle-menu-bar ()
  (if olivetti-mode
      (progn
        ;; Save current state
        (setq my/olivetti-menu-bar-was-enabled menu-bar-mode)
        ;; Turn it off
        (menu-bar-mode -1))
    ;; Restore previous state
    (when my/olivetti-menu-bar-was-enabled
      (menu-bar-mode 1))))

(add-hook 'olivetti-mode-hook #'my/olivetti-toggle-menu-bar)
(add-hook 'olivetti-mode-hook #'my/olivetti-mode-setup)

;; --------------------------
;; Markdown Mode
;; --------------------------
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc"))

;; --------------------------
;; Some Handy Functions
;; --------------------------
(defun my/insert-date ()
  "Insert today's date as 'Month D, YYYY' (e.g., 'November 6, 2025')."
  (interactive)
  (let ((s (format-time-string "%B %e, %Y")))
    ;; Collapse the space-padding from %e for single-digit days.
    (insert (replace-regexp-in-string "  +" " " s))))

;; Bind it
(when (fboundp 'keymap-global-set)
  (keymap-global-set "C-c d" #'my/insert-date))
