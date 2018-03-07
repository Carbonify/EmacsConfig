;; Emacs init file
(setq gc-cons-threshold (* 20 1024 1024))


(add-to-list 'load-path "~/.emacs.d/manual-install/") ;; Add dir for manually installed plugins
(add-to-list 'load-path "~/.emacs.d/config-lisp/") ;; Add more config by me.
(load-file "~/.emacs.d/config-lisp/ftdetect.el") ;; Load the file that detects filetypes
(load-file "~/.emacs.d/macros.el") ;; Load keyboard macros
;; Then, load all autoloads for my config
(load-file "~/.emacs.d/config-lisp/autoloads.el")
;;(load-file "~/.emacs.d/manual-install/autoloads.el")

;; Packages -------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; General settings ------------------

;; Change backup directory to system temp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq abbrev-file-name "~/.emacs.d/abbrev_defs") ;; Change default abbreviations file
(setq-default abbrev-mode t) ;; Enable abbrev-mode by default
(global-hl-line-mode) ;; Highlight current cursor line
(scroll-bar-mode -1) ;; Disable the scroll bar
(prefer-coding-system 'utf-8) ;; Prefer UTF-8 encoding
(electric-indent-mode 1) ;; Always make newline keep indent
(electric-pair-mode 1) ;; Pair parens and other brackets
(icomplete-mode 1) ;; Incremental completion in minibuffers
(setq-default word-wrap t) ;; Wrap at word ends instead of in the middle of a word.
(setq save-interprogram-paste-before-kill t) ;; Save the clipboard to kill ring
(setq lazy-highlight-initial-delay 3)
(setq lazy-highlight-max-at-a-time 35)
(setq custom-file "~/.emacs.d/custom.el") ;; Change customization save file.
(load custom-file)
(put 'narrow-to-region 'disabled nil) ;; Enable narrow commands.
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'downcase-region  'disabled nil)
(make-variable-buffer-local 'hippie-expand-try-functions-list)
(setq kmacro-execute-before-append nil) ;; Make macros not execute before appending with C-u F3
(setq delete-by-moving-to-trash t) ;; Make dired not delete files permenently
(defalias 'yes-or-no-p 'y-or-n-p) ;; Make prompt dialogue shorter.
(setq inhibit-startup-message t)  ;; Disable the standard startup screen, replacing it with something better.
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "Blank scratch buffer for experiments or short volatile notes.")
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))
(setq-default tab-width 4) ;; Smaller default tab width
(setq search-whitespace-regexp "[_- \\n]")
(setq-default indent-tabs-mode nil)


;; Scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


;; Bookmarking
(require 'bookmark)
(setq bookmark-default-file "~/.emacs.d/bookmarks.txt")
(setq bookmark-save-flag t) ; save bookmark when emacs quits
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

;; Theme
(load-theme 'misterioso)

;; IDO mode
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order '(".txt" ".cfg" ".el" ".sh" ".json" ".md")) ;; Emphasis
(setq ido-ignore-extensions t)

;; Plugin config -----------------

(load "plugin-config")

;; Hooks ------------------------

;; Text mode hooks

(add-hook 'text-mode-hook 'flyspell-mode) ;; Turn on incorrect spell highlight
(add-hook 'text-mode-hook '(lambda () (interactive) (setq sentence-end-double-space nil)))
(defun user--text-hippie-expand-setting () (interactive)
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-all-abbrevs)))
(add-hook 'text-mode-hook 'user--text-hippie-expand-setting)

;; Elisp mode hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(defun user--prog-lisp-hippie-expand-setting () (interactive)
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-dabbrev try-expand-dabbrev-all-buffers)))
(add-hook 'emacs-lisp-mode-hook 'user--prog-lisp-hippie-expand-setting)


;; Org mode hooks
(add-hook 'org-mode-hook 'visual-line-mode) ;; Make org just wrap long lines
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook '(lambda ()
                            (setq fill-column 100)))

;; Programming mode hooks
(defun user--prog-hippie-expand-setting () (interactive)
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name try-expand-dabbrev try-expand-dabbrev-all-buffers)))
(add-hook 'prog-mode-hook 'user--prog-hippie-expand-setting)

;; On buffer save hooks
(add-hook 'before-save-hook 'user--clean-buffer)

;; Manual pages
(add-hook 'man-mode-hook 'visual-line-mode)

;; Ibuffer mode
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode t)
             (ibuffer-switch-to-saved-filter-groups "Standard"))) ;; See plugin config for definition

;; Keybindings ---------------------------------
(load "misc-functions")
(load "keybindings")

;; Finalization ----------------------

(server-start) ;; Start the server in this instance, so emacs doesn't have to open again

(setq gc-cons-threshold 800000) ;;Fix value back to it's default.
