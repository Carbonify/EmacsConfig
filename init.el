;; Emacs init file
(setq gc-cons-threshold (* 20 1024 1024))


;; Declare running OS
(message "Init emacs...")
(cond
 ((string-equal system-type "windows-nt")
  (progn
    (message "Running on Microsoft Windows")))
 ((string-equal system-type "darwin") ;  macOS
  (progn
    (message "Running on Mac OS X")))
 ((string-equal system-type "gnu/linux")
  (progn
    (message "Running on Linux"))))


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

;; Change backup directory to system temp directory, or in same directory if on windows
;; Same dir on windows because windows is bad at having a temp dir accessible without admin perms
(if (string-equal system-type "windows-nt")
    (setq backup-directory-alist nil)
 (setq backup-directory-alist `((".*" . ,temporary-file-directory))))

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
(setq auto-window-vscroll nil) ;; Fix scrolling performance issues
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

;; Use hunspell if running on windows
(if (string-equal system-type "windows-nt")
    (setq-default ispell-program-name "C:/Program Files (x86)/Hunspell/bin/hunspell.exe"))


;; Bookmarks
(require 'bookmark)
(setq bookmark-default-file "~/.emacs.d/bookmarks.txt")
(setq bookmark-save-flag t) ; save bookmark when emacs quits
(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")


;; Scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)


;; IDO mode
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order '(".txt" ".cfg" ".el" ".sh" ".json" ".md")) ;; Emphasis
(setq ido-ignore-extensions t)

;; Plugin config -----------------

(load "plugin-config")

;; Hooks ------------------------

(load "mode-hooks")


;; Keybindings ---------------------------------
(load "misc-functions")
(load "keybindings")

;; Finalization ----------------------

;; Emacsclient stuff
(server-start) ;; Start the server in this instance, so emacs doesn't have to open again

;; Last second settings ------
(setq ring-bell-function 'user--visual-bell-flash-modeline)

(setq gc-cons-threshold 800000) ;;Fix value back to it's default.
