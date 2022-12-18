;;; This file contains various plugin config, including options,
;;; keybindings specific to plugins, etc. It also enables certain
;;; Global modes that make plugins operate.

(use-package mediawiki-mode
  :mode "\\.[Mm][wW]\\'")

(use-package ess
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((c-mode   . lsp)
	       (c++-mode . lsp))
  :commands lsp
  :config
  (unless (executable-find "clangd")
    (message "Clangd is not installed or available, LSP will not work."))
  (unless (executable-find "bear")
    (message "Bear is not installed or available, LSP may not work."))
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-idle-delay 0.1)
  (setq lsp-enable-snippet nil)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-file-watch-threshold 15000))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands (lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-delay 0.5)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-ivy
  :ensure t
  :after (ivy lsp-mode)
  :commands lsp-ivy-workspace-symbol)

(use-package solarized-theme
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  ;; Remove saving of undo history files
  (setq undo-tree-auto-save-history nil))

(use-package iedit
  :ensure t
  :bind ("M-m" . iedit-mode))


(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-timer)
         ("C-'" . avy-kill-region)
         ("C-:" . avy-kill-whole-line)))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.6) ;; How long before it pops up the completion automatically
  (setq company-selection-wrap-around t) ;; List loops upon reaching bottom
  :bind (("TAB" . company-indent-or-complete-common)
         :map company-active-map
         ("<tab>" . company-select-next-if-tooltip-visible-or-complete-selection)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package markdown-mode
  :ensure t
  :mode "\\.[Mm][dD]\\'")

(use-package avy-zap
  :ensure t
  :after avy
  :bind ("M-z" . avy-zap-up-to-char))


(use-package flyspell
  :ensure t
  :hook (text-mode . flyspell-mode)
  ;;disables the binding so avy can use it
  :bind (:map flyspell-mode-map
              ("C-;" . nil)
              ("C-," . nil)))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :ensure t
  :hook (prog-mode . projectile-mode)
  :bind ("C-x p" . projectile-command-map)
  :after ivy
  :config
  (setq-default projectile-completion-system 'ivy))


(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;; Ivy (completion framework)
(use-package ivy
  :ensure t
  :demand t
  ;; Re-open ivy where we last left off, in case of accidental accept
  :bind ("C-c r" . ivy-resume)
  :config
  (ivy-mode)
  (setq-default ivy-use-virtual-buffers t) ;; Don't spam up buffer list with temp buffers
  (setq enable-recursive-minibuffers t)
  ;; Wrap around in matches list
  (setq-default ivy-wrap t))

(use-package flx
  :ensure t
  :after ivy
  :config

  ;; Fuzzy matching for ivy, EXCEPT for searching in files as that can cause
  ;; too many things to pop up (note: you should have flx installed for better popup
  ;; ordering - better matches rise to the top)
  (setq-default ivy-re-builders-alist
                '((counsel-grep-or-swiper . ivy--regex-plus)
                  (swiper . ivy--regex-plus)
                  (t . ivy--regex-fuzzy))))

(use-package ivy-xref
  :ensure t
  :after ivy
  :config
  (when (>= emacs-major-version 27)
    (setq-default xref-show-definitions-function #'ivy-xref-show-defs))
  (setq-default xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :ensure t
  :demand t
  :bind ("C-," . counsel-mark-ring)
  :config
  (counsel-mode))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

;; Mosey
(use-package mosey
  :ensure t
  :bind (("C-a" . mosey-backward-bounce)
         ("C-e" . mosey-forward-bounce)))


(require 'windmove)
(windmove-default-keybindings)

;;lua mode
(use-package lua-mode
  :ensure t
  :config
  (setq-default lua-indent-level 4))

;; Eshell
(setq-default eshell-prompt-function
              (lambda ()
                (concat
                 (propertize "┌─[" 'face `(:foreground "purple"))
                 (propertize (user-login-name) 'face `(:foreground "red"))
                 (propertize "@" 'face `(:foreground "purple"))
                 (propertize (system-name) 'face `(:foreground "blue"))
                 (propertize "]──[" 'face `(:foreground "purple"))
                 (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "orange"))
                 (propertize "]──[" 'face `(:foreground "purple"))
                 (propertize (concat (eshell/pwd)) 'face `(:foreground "black"))
                 (propertize "]\n" 'face `(:foreground "purple"))
                 (propertize "└─>" 'face `(:foreground "purple"))
                 (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "purple"))
                 )))

;; Ibuffer
(eval-after-load 'ibuffer
  '(progn
     (setq-default ibuffer-show-empty-filter-groups nil)
     (setq-default ibuffer-saved-filter-groups
                   (quote (("Standard"
                            ("Emacs Config" (mode . emacs-lisp-mode)) ;; Elisp files
                            ("Text" (or (mode . text-mode) ;; All kinds of prose, non-org
                                        (mode . markdown-mode)
                                        (mode . mediawiki-mode)))
                            ("Dired" (mode . dired-mode))
                            ("Org" (mode . org-mode))
                            ("Documentation & Docs" (or (mode . Info-mode) ;; Built in emacs docs
                                                        (mode . man-mode)  ;; Man pages
                                                        (mode . doc-view-mode) ;; PDF files
                                                        (mode . help-mode)))
                            ("GDB" (mode . gdb-parent-mode)) ;; Debugger
                            ("Images" (mode . image-mode)) ;; Image files viewed in emacs
                            ("Programming" (or (mode . javascript-mode)
                                               (mode . makefile-gmake-mode)
                                               (mode . makefile-mode)
                                               (mode . c++-mode)
                                               (mode . rust-mode)
                                               (mode . c-mode)
                                               (mode . lua-mode)))
                            ("Special Buffer" (name . "^\\*.*\\*$")))))))) ;; Temp or unbound buffers

(use-package org
  :ensure t
  :hook ((org-mode . visual-line-mode)
         (org-mode . auto-fill-mode)
         (org-mode . electric-indent-local-mode))
  :config
  (setq-default org-return-follows-link t)
  (setq-default org-log-done t))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package paredit
  :hook (prog-mode . enable-paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") 'paredit-newline)
  (define-key paredit-mode-map (kbd "C-j") nil))


;; end
