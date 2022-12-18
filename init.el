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
(load-file "~/.emacs.d/macros.el") ;; Load keyboard macros

;; Packages -------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
(electric-indent-mode -1) ;; Disable electric indent
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
(setq help-window-select t) ;; auto-swap to newly made help windows
(setq sentence-end-double-space nil) ;; Don't use outdated double space after periods
(setq kill-ring-max (* kill-ring-max 2)) ;; Double kill ring max size


;; File encoding nonsense
(if (eq system-type 'windows-nt)
    (progn
      (set-clipboard-coding-system 'utf-16-le)
      (set-selection-coding-system 'utf-16-le))
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)
(when (display-graphic-p) ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


;; indentation
(setq-default tab-width 2) ;; Smaller default tab width
(setq-default indent-tabs-mode nil) ;; No tabs for indenting
(defvaralias 'c-basic-offset 'tab-width) ;; set these vars to track tab-width
(defvaralias 'cperl-indent-level 'tab-width)


;; Use hunspell if running on windows
(when (string-equal system-type "windows-nt")
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


;; Other config files ------------

(load "plugin-config")
(load "mode-hooks")
(load "skeletons")
(load "digraphs")
(load "misc-functions")
(load "keybindings")

;; Finalization ----------------------

;; Start the server in this instance, so emacs doesn't have to open again
(server-start)

;; Graphics, themes, etc
(load-theme 'solarized-light t)
(setq-default solarized-distinct-fringe-background t)

;; Last second settings ------
(setq ring-bell-function 'nm-visual-bell-flash-modeline)

(setq gc-cons-threshold 800000) ;;Fix value back to it's default.
