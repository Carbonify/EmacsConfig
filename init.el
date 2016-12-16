;;Initial setup that was created via GUI.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 '(save-place t nil (saveplace))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 '(default ((t (:family "Terminus (TTF)" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

;; General settings ------------------

;; Change backup directory to system temp directory
 (setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
;; Change default abbreviations file
 (setq abbrev-file-name "~/.emacs.d/abbrev_defs")  
;; Enable abbrev-mode by default
(setq-default abbrev-mode t)




;; Hooks ------------------------
;; Text mode hooks
(add-hook 'text-mode-hook 'flyspell-mode) ;; Turn on incorrect spell highlight



;; Keybindings ---------------------------------

;; Use Ctrl-C e to open the init file for changing config.
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
