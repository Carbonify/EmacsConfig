;; Emacs init file

;; General settings ------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(save-place t nil (saveplace))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
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

;; Mode creation ------------------------------

(define-derived-mode mediawiki-mode text-mode "Mediawiki"
  "Major mode for Mediawiki articles. 
   \\{mediawiki-mode-map}"
  :abbrev-table nil
  (defun create-mediawiki-table () "Creates a mediawiki table."
      (interactive)
      (insert "{| class=\"wikitable\"") (newline)
      (insert "|-") (newline)
      (insert "!Option1!!Option2!!Option3") (newline)
      (insert "|-") (newline)
      (insert "|Value1 || Value2 || Value3") (newline)
      (insert "|-") (newline) 
      (insert "|}"))

  (defun create-mediawiki-collapse () "Creates the HTML source code for a collapsible text field."
    (interactive)
    (insert "<div class=\"toccolours mw-collapsible mw-collapsed\" style=\"width:800px\">") (newline)
    (insert "Shown content") (newline)
    (insert "<div class=\"mw-collapsible-content\">") (newline)
    (insert "Hidden Content") (newline)
    (insert "</div></div>") (newline))


  (define-key mediawiki-mode-map (kbd "C-c C-w t") 'create-mediawiki-table)
  (define-key mediawiki-mode-map (kbd "C-c C-w c") 'create-mediawiki-collapse))


(define-derived-mode markdown-mode text-mode "Markdown"
  "Major mode for markdown documents.
\\{markdown-mode-map}"
  :abbrev-table nil)


;; Hooks ------------------------
;; Text mode hooks
(add-hook 'text-mode-hook 'flyspell-mode) ;; Turn on incorrect spell highlight

;;Elisp mode hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)


;; Keybindings ---------------------------------

;; Use Ctrl-C e to open the init file for changing config.
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Filetype detection --------------------

;; Text-type modes
(add-to-list 'auto-mode-alist '("\\.mw\\'" . mediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . mediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))


;; Finalization ----------------------

(server-start) ;; Start the server in this instance, so emacs doesn't have to open again

