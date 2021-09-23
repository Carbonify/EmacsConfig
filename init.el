;; Emacs init file
(setq gc-cons-threshold (* 20 1024 1024))


;; Declare running OS
(message "Init emacs...")
(cond
 ((string-equal system-type "windows-nt")
  (progn
    (message "Running on Microsoft Windows")))
 ((string-equal system-type "darwin")
  (progn
    (message "Running on Mac OS X")))
 ((string-equal system-type "gnu/linux")
  (progn
    (message "Running on Linux"))))


(add-to-list 'load-path "~/.emacs.d/config-lisp/") ;; Add more config by me.
(load "ftdetect") ;; Load the file that detects filetypes
(load-file "~/.emacs.d/macros.el") ;; Load keyboard macros
(load-file "~/.emacs.d/config-lisp/autoloads.el") ;; Autoloads part of config lisp


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
(setq kmacro-execute-before-append nil) ;; Make macros not execute before appending with C-u F3
(setq delete-by-moving-to-trash t) ;; Make dired not delete files permenently
(defalias 'yes-or-no-p 'y-or-n-p) ;; Make prompt dialogue shorter.
(setq inhibit-startup-message t)  ;; Disable the standard startup screen, replacing it with something better.
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "Blank scratch buffer for experiments or short volatile notes.")
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))
(setq search-whitespace-regexp "[_- \\n]")
(require 'uniquify) ;; //Use alternative unique buffer naming scheme

;; indentation
(setq-default tab-width 2) ;; Smaller default tab width
(setq-default indent-tabs-mode nil) ;; No tabs for indenting
(defvaralias 'c-basic-offset 'tab-width) ;; set these vars to track tab-width
(defvaralias 'cperl-indent-level 'tab-width)


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


;; Plugin config, manual install loads -----------------

(load "plugin-config")
(load "digraphs")

;; Hooks ------------------------

(load "mode-hooks")

;; Skeletons
(load "skeletons")


;; Keybindings ---------------------------------
(load "misc-functions")
(load "keybindings")

;; Finalization ----------------------

;; Emacsclient stuff
(server-start) ;; Start the server in this instance, so emacs doesn't have to open again

;; Graphics, themes, etc
(load-theme 'solarized-light t)
(setq-default solarized-distinct-fringe-background t)


;; Last second settings ------
(setq ring-bell-function 'nm-visual-bell-flash-modeline)

(setq gc-cons-threshold 800000) ;;Fix value back to it's default.
