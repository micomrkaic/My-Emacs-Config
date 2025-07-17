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
;;(setq dashboard-startup-banner 'official)
(setq dashboard-startup-banner "~/.emacs.d/morpheus-small.png")
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
;; Font and Ligature Settings
;; --------------------------
(set-face-attribute 'default nil
  :family "Iosevka"
  :height 120)  ;; adjust to your preferred size (100 = 10pt, 110 = 11pt, etc.)

(defun my-c-mode-comment-italics ()
  (set-face-attribute 'font-lock-comment-face nil
    :slant 'italic))

(add-hook 'c-mode-hook #'my-c-mode-comment-italics)
(add-hook 'c++-mode-hook #'my-c-mode-comment-italics)

(defun my-elisp-comment-italics ()
  (set-face-attribute 'font-lock-comment-face nil
    :slant 'italic))

(add-hook 'emacs-lisp-mode-hook #'my-elisp-comment-italics)

;; Load ligature.el from local path
(add-to-list 'load-path "~/.emacs.d/ligature")
(require 'ligature)

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

;; Save font and line-number state
(defvar-local my/olivetti-font-override nil)

;; --------------------------
;; Olivetti Mode Fine Tuning
;; --------------------------
(defun my/olivetti-mode-setup ()
  "Adjust font and line numbers for Olivetti mode."
  (if olivetti-mode
      (progn
        ;; Disable line numbers
        (display-line-numbers-mode 0)
        ;; Change font to Charis SIL
        (setq my/olivetti-font-override
              (face-remap-add-relative 'default :family "Charis SIL" :height 140)))
    ;; When Olivetti mode is turned off
    (display-line-numbers-mode 1)
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

