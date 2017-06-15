
;;Avy
(require 'avy) ;; Always load avy
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-'") 'avy-kill-region)
(global-set-key (kbd "C-:") 'avy-kill-whole-line)


;; Avy zap up to char
(global-set-key (kbd "M-z") #'avy-zap-up-to-char)

;; Flyspell
(eval-after-load 'flyspell '(define-key flyspell-mode-map (kbd "C-;") nil)) ;;disables the binding so avy can use it

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;; Smart mode line
(setq sml/theme 'dark)
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
         (propertize "┌─[" 'face `(:foreground "green"))
         (propertize (user-login-name) 'face `(:foreground "red"))
         (propertize "@" 'face `(:foreground "green"))
         (propertize (system-name) 'face `(:foreground "blue"))
         (propertize "]──[" 'face `(:foreground "green"))
         (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
         (propertize "]──[" 'face `(:foreground "green"))
         (propertize (concat (eshell/pwd)) 'face `(:foreground "white"))
         (propertize "]\n" 'face `(:foreground "green"))
         (propertize "└─>" 'face `(:foreground "green"))
         (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "green"))
         )))
