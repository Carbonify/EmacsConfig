
;;Avy
(require 'avy) ;; Always load avy
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-'") 'avy-kill-region)
(global-set-key (kbd "C-:") 'avy-kill-whole-line)

;; Company mode ---------
(require 'company)
(global-company-mode)
(setq company-idle-delay 0.6)
(setq company-selection-wrap-around t) ;; List loops upon reaching bottom


;; Make tab accept the selected completion
(define-key company-active-map (kbd "<tab>") 'company-select-next-if-tooltip-visible-or-complete-selection)


;; Avy zap up to char --------
(global-set-key (kbd "M-z") #'avy-zap-up-to-char)

;; Flyspell  ------
(eval-after-load 'flyspell '(define-key flyspell-mode-map (kbd "C-;") nil)) ;;disables the binding so avy can use it

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Projectile
(eval-after-load "projectile"
    '(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

;; Smart mode line
(setq sml/theme 'light)
(sml/setup)


;; Smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; Windmove
(require 'windmove)
(windmove-default-keybindings)

;;lua mode
(eval-after-load 'lua-mode
  (progn
    (setq lua-indent-level 4)
    ))

;; Eshell
(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "┌─[" 'face `(:foreground "purple"))
         (propertize (user-login-name) 'face `(:foreground "red"))
         (propertize "@" 'face `(:foreground "purple"))
         (propertize (system-name) 'face `(:foreground "blue"))
         (propertize "]──[" 'face `(:foreground "purple"))
         (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "orange"))
         (propertize "]──[" 'face `(:foreground "purple"))
         (propertize (concat (eshell/pwd)) 'face `(:foreground "black"))
         (propertize "]\n" 'face `(:foreground "purple"))
         (propertize "└─>" 'face `(:foreground "purple"))
         (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "purple"))
         )))

;; Ibuffer
(eval-after-load 'ibuffer
  '(progn
     (setq ibuffer-show-empty-filter-groups nil)
     (setq ibuffer-saved-filter-groups
           (quote (("Standard"
                    ("ELisp" (mode . emacs-lisp-mode))
                    ("Text" (or (mode . text-mode)
                                (mode . markdown-mode)
                                (mode . mediawiki-mode)))
                    ("Dired" (mode . dired-mode))
                    ("Org" (mode . org-mode))
                    ("Programming" (or (mode . javascript-mode)
                                       (mode . c++-mode)
                                       (mode . lua-mode)))
                    ("Special Buffer" (name . "^\\*.*\\*$"))))))))

;; Org
(eval-after-load 'org
  '(progn
     (define-key org-mode-map [tab] 'hippie-expand) ;; Set this, because I use tab complete much more than cycle
     (setq org-log-done t)))

;; Racer (Rust prg)
(eval-after-load 'rust-mode
  '(progn
     (setq racer-cmd "~/.cargo/bin/racer")))
