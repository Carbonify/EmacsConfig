;;Initial setup that was created via GUI.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 '(save-place t nil (saveplace))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 '(default ((t (:family "Terminus (TTF)" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

;; Begin normal setup

;; General settings ------------------

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


;; Hooks ------------------------
;; Text mode hooks
(add-hook 'text-mode-hook 'flyspell-mode) ;; Turn on incorrect spell highlight

;;Elisp mode hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)


;; Keybindings ---------------------------------

;; Use Ctrl-C e to open the init file for changing config.
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Major mode definitions -------------------------
(define-derived-mode mediawiki-mode text-mode "Mediawiki"
  "Major mode for mediawiki articles. \\{mediawiki-mode-map}"
  'abbrev-table "mediawiki-mode-abbrev-table")

