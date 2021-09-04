;; Text mode hooks

(add-hook 'text-mode-hook 'flyspell-mode) ;; Turn on incorrect spell highlight
(add-hook 'text-mode-hook '(lambda () (setq-local sentence-end-double-space nil)))

;; Elisp mode hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Org mode hooks
(add-hook 'org-mode-hook 'visual-line-mode) ;; Make org just wrap long lines
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook '(lambda ()
                            (setq-local fill-column 100)))

;; Programming mode hooks
(add-hook 'prog-mode-hook 'flycheck-mode) ;; Enable flycheck for programming

;; On buffer save hooks
(add-hook 'before-save-hook 'user--clean-buffer)

;; Manual pages
(add-hook 'man-mode-hook 'visual-line-mode)

;; Ibuffer mode
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode t)
             (ibuffer-switch-to-saved-filter-groups "Standard"))) ;; See plugin config for definition

;; Rust mode
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'eldoc-mode)
;; Bind reindent to instead run rustfmt, as it can do it better
(add-hook 'rust-mode-hook (lambda () (local-set-key (kbd "C-c i") #'rust-format-buffer)))
(add-hook 'rust-mode-hook #'flycheck-rust-setup)


;; C++ mode hooks
(add-hook 'c++-mode-hook 'linum-mode)
