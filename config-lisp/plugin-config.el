;;; This file contains various plugin config, including options,
;;; keybindings specific to plugins, etc. It also enables certain
;;; Global modes that make plugins operate.


;;Avy
(require 'avy) ;; Always load avy
(global-set-key (kbd "C-;") 'avy-goto-char-timer)
(global-set-key (kbd "C-'") 'avy-kill-region)
(global-set-key (kbd "C-:") 'avy-kill-whole-line)

;; Company mode (text completion framework) ---------
(require 'company)
(global-company-mode)
(setq company-idle-delay 0.6) ;; How long before it pops up the completion automatically
(setq company-selection-wrap-around t) ;; List loops upon reaching bottom
;; Make tab accept the selected completion, like how IDE completion works
(define-key company-active-map (kbd "<tab>") 'company-select-next-if-tooltip-visible-or-complete-selection)


;; Avy zap up to char --------
(global-set-key (kbd "M-z") #'avy-zap-up-to-char)

;; Flyspell  ------
(eval-after-load 'flyspell '(define-key flyspell-mode-map (kbd "C-;") nil)) ;;disables the binding so avy can use it

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Projectile (project handler, mostly used for switching between header and code files)
(eval-after-load "projectile"
  '(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))
(setq-default projectile-completion-system 'ivy)


;; mode line (using mood-line)
(mood-line-mode)


;; Ivy (completion framework)
(ivy-mode)
(setq-default ivy-use-virtual-buffers t) ;; Don't spam up buffer list with temp buffers
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c r") 'ivy-resume) ;; Re-open ivy where we last left off, in case of accidental accept
(setq-default ivy-wrap t) ;; Wrap around in matches list

;; Fuzzy matching for ivy, EXCEPT for searching in files as that can cause
;; too many things to pop up (note: you should have flx installed for better popup
;; ordering - better matches rise to the top)
(setq-default ivy-re-builders-alist
              '((counsel-grep-or-swiper . ivy--regex-plus)
                (swiper . ivy--regex-plus)
                (t . ivy--regex-fuzzy)))


;; Counsel (command usefulness booster)
(counsel-mode)

;; Swiper
(global-set-key (kbd "C-s") 'swiper)


;; Windmove
(require 'windmove)
(windmove-default-keybindings)

;;lua mode
(setq-default lua-indent-level 4)


;; Eshell
(setq-default eshell-prompt-function
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
     (setq-default ibuffer-show-empty-filter-groups nil)
     (setq-default ibuffer-saved-filter-groups
                   (quote (("Standard"
                            ("Emacs Config" (mode . emacs-lisp-mode))
                            ("Text" (or (mode . text-mode)
                                        (mode . markdown-mode)
                                        (mode . mediawiki-mode)))
                            ("Dired" (mode . dired-mode))
                            ("Org" (mode . org-mode))
                            ("Documentation" (or (mode . Info-mode)
                                                 (mode . man-mode)
                                                 (mode . help-mode)))
                            ("Programming" (or (mode . javascript-mode)
                                               (mode . c++-mode)
                                               (mode . c-mode)
                                               (mode . lua-mode)))
                            ("Special Buffer" (name . "^\\*.*\\*$"))))))))

;; Org
(eval-after-load 'org
  '(progn
     (setq org-log-done t)))

;; Racer (Rust prg)
(eval-after-load 'rust-mode
  '(progn
     (setq-default racer-cmd "~/.cargo/bin/racer")))
