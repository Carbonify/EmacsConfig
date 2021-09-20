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
(defun user--rust-mode-config ()
  "Runs several things to config the mode under one hook."
  ;; Bind reindent to instead run rustfmt, as it can do it better
  (local-set-key (kbd "C-c i") #'rust-format-buffer))

(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'eldoc-mode)

(add-hook 'rust-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'user--rust-mode-config)


;; C++ mode hooks
(defun user--c++-mode-config ()
  "Runs several things to config the mode under one hook."
  ;; fix beginning of defun not working on some laptops
  (local-unset-key (kbd "C-M-a"))
  (local-set-key (kbd "C-M-q") 'c-beginning-of-defun))

(add-hook 'c++-mode-hook 'linum-mode)
(add-hook 'c++-mode-hook 'projectile-mode)
(add-hook 'c++-mode-hook 'subword-mode)
(add-hook 'c++-mode-hook 'user--c++-mode-config)
