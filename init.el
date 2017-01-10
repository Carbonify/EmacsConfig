;; Emacs init file

(add-to-list 'load-path "~/.emacs.d/manual-install/") ;; Add dir for manually installed plugins
(add-to-list 'load-path "~/.emacs.d/config-lisp/") ;; Add more config by me.
(load-file "~/.emacs.d/config-lisp/ftdetect.el") ;; Load the file that detects filetypes
;; Then, load all autoloads
(load-file "~/.emacs.d/config-lisp/autoloads.el")
;;(load-file "~/.emacs.d/manual-install/autoloads.el")

;; Packages -------------------------
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
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
(setq-default word-wrap t) ;; Wrap at word ends instead of in the middle of a word.
(setq save-interprogram-paste-before-kill t) ;; Save the clipboard to kill ring
(setq lazy-highlight-initial-delay 1.5)
(setq lazy-highlight-max-at-a-time 35)
(setq custom-file "~/.emacs.d/custom.el") ;; Change customization save file.
(load custom-file)
(put 'narrow-to-region 'disabled nil) ;; Enable narrow commands.
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(make-variable-buffer-local 'hippie-expand-try-functions-list)


;; IDO mode
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order '(".txt" ".cfg" ".el" ".sh" ".json" ".md")) ;; Emphasis
(setq ido-ignore-extensions t)


;; Mode creation ------------------------------


;; Plugin config -----------------

;;Avy
(require 'avy) ;; Always load avy
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

;; Flyspell
(eval-after-load 'flyspell '(define-key flyspell-mode-map (kbd "C-;") nil))


;; Hooks ------------------------

;; Text mode hooks

(add-hook 'text-mode-hook 'flyspell-mode) ;; Turn on incorrect spell highlight
(add-hook 'text-mode-hook '(lambda () (interactive) (setq sentence-end-double-space nil)))
(defun user--text-hippie-expand-setting () (interactive)
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-all-abbrevs)))
(add-hook 'text-mode-hook 'user--text-hippie-expand-setting)

;; Elisp mode hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Org mode hooks
(add-hook 'org-mode-hook '(lambda ()
(define-key org-mode-map [tab] 'hippie-expand) ;; Set these two, because I use tab complete much more than cycle
(define-key org-mode-map [home] 'org-cycle)))
(add-hook 'org-mode-hook 'visual-line-mode) ;; Make org just wrap long lines

;; Programming mode hooks
(defun user--prog-hippie-expand-setting () (interactive)
  (setq hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
(add-hook 'prog-mode-hook 'user--prog-hippie-expand-setting)


;; Keybindings ---------------------------------

;; Use Ctrl-C e to open the init file for changing config.
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Tab completion
(global-set-key (kbd "<tab>") 'hippie-expand)
(global-set-key (kbd "C-<tab>") 'indent-for-tab-command) ;; Old function of tab

;; Make using ISearch much easier
(define-key isearch-mode-map [next] 'isearch-repeat-forward)
(define-key isearch-mode-map [prior] 'isearch-repeat-backward)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Search all loaded buffers for a regex
(defun user--search-all-buffers (regexp) "Search all open buffers for a regex. Open an occur-like window."
  (interactive "sRegexp: ")
  (multi-occur-in-matching-buffers "." regexp t))
(global-set-key [f7] 'user--search-all-buffers)

;; Misc binds
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; Interactive buffer switch

;; Finalization ----------------------

(server-start) ;; Start the server in this instance, so emacs doesn't have to open again
