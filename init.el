;; Emacs init file

;; General settings ------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-all-abbrevs try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(save-place t nil (saveplace))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Terminus (TTF)" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

;; Change backup directory to system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(setq abbrev-file-name "~/.emacs.d/abbrev_defs") ;; Change default abbreviations file
(setq-default abbrev-mode t) ;; Enable abbrev-mode by default
(global-hl-line-mode) ;; Highlight current cursor line
(scroll-bar-mode -1) ;; Disable the scroll bar
(prefer-coding-system 'utf-8) ;; Prefer UTF-8 encoding
(add-to-list 'load-path "~/.emacs.d/manual-install/") ;; Add dir for manually installed plugins
(add-to-list 'load-path "~/.emacs.d/config-lisp/") ;; Contains more config by me.
(electric-indent-mode 1) ;; Always make newline keep indent
(electric-pair-mode 1) ;; Pair parens and other brackets


;; Mode creation ------------------------------

(define-derived-mode markdown-mode text-mode "Markdown"
  "Major mode for markdown documents.
\\{markdown-mode-map}"
  :abbrev-table nil)

;; Plugin config -----------------

;; Mediawiki-mode (manual-install)
(require 'mediawiki-mode)


;; Hooks ------------------------
;; Text mode hooks
(add-hook 'text-mode-hook 'flyspell-mode) ;; Turn on incorrect spell highlight

;;Elisp mode hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)


;; Keybindings ---------------------------------

;; Use Ctrl-C e to open the init file for changing config.
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Tab completion
(global-set-key (kbd "<tab>") 'hippie-expand)
(global-set-key (kbd "C-<tab>") 'indent-for-tab-command) ;; Old function of tab

;; Filetype detection --------------------

;; Text-type modes

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))


;; Finalization ----------------------

(server-start) ;; Start the server in this instance, so emacs doesn't have to open again

