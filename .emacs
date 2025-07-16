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
;; Font Settings
;; --------------------------
;;(set-face-attribute 'default nil :font "IBM Plex Mono Medium")
(set-face-attribute 'default nil :font "Victor Mono-12")

;; (set-face-attribute 'default nil
;;   :family "Fira Code"
;;   :weight 'medium
;;   :height 120)  ;; Adjust size (100 = 10pt)

;; (set-face-attribute 'default nil
;;   :family "JetBrains Mono"
;;   :height 110)  adjust to your preferred size (100 = 10pt, 110 = 11pt, etc.)

(defun my-c-mode-comment-italics ()
  (set-face-attribute 'font-lock-comment-face nil
    :slant 'italic))

(add-hook 'c-mode-hook #'my-c-mode-comment-italics)
(add-hook 'c++-mode-hook #'my-c-mode-comment-italics)

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
;; Font Configuration by Mode
;; --------------------------
(defun my/set-prog-font ()
  "Use Victor Mono for all programming modes."
  (when (member "Victor Mono" (font-family-list))
    (set-face-attribute 'default nil :family "Victor Mono" :height 120)))

(defun my/set-writing-font ()
  "Use EB Garamond for prose in Org and Markdown, and Victor Mono for fixed-pitch text."
  (when (member "IBM Plex Mono Medium" (font-family-list))
    (face-remap-add-relative 'default :family "IBM Plex Mono Medium" :height 120)
    (face-remap-add-relative 'fixed-pitch :family "Victor Mono" :height 120)))

(add-hook 'prog-mode-hook #'my/set-prog-font)
(add-hook 'org-mode-hook #'my/set-writing-font)
(add-hook 'markdown-mode-hook #'my/set-writing-font)

;; Fallback to programming font on startup
(my/set-prog-font)

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

;;(setq display-line-numbers-type 'relative)
;;(global-display-line-numbers-mode)


;; Don't enable line numbers globally
;; We'll handle it buffer-by-buffer
(setq-default display-line-numbers nil)

(defun my/enable-line-numbers-based-on-mode ()
  "Enable relative line numbers in programming modes, absolute otherwise."
  (setq display-line-numbers-type
        (if (derived-mode-p 'prog-mode)
            'relative
          t))
  (display-line-numbers-mode 1))

;; Apply on all buffers
(add-hook 'after-change-major-mode-hook #'my/enable-line-numbers-based-on-mode)

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

