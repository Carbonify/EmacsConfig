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
(add-to-list 'load-path "~/.emacs.d/manual-install/") ;; Add dir for manually installed plugins
(electric-indent-mode 1) ;; Always make newline keep indent
(electric-pair-mode 1) ;; Pair parens and other brackets


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

;; Plugin config -----------------


;; Hooks ------------------------
;; Text mode hooks
(add-hook 'text-mode-hook 'flyspell-mode) ;; Turn on incorrect spell highlight

;;Elisp mode hooks
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)


;; Keybindings ---------------------------------

;; Use Ctrl-C e to open the init file for changing config.
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Tab complete
(global-set-key [(tab)] 'smart-tab)
(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (dabbrev-expand nil)
        (indent-for-tab-command)))))


;; Filetype detection --------------------

;; Text-type modes
(add-to-list 'auto-mode-alist '("\\.mw\\'" . mediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . mediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))


;; Finalization ----------------------

(server-start) ;; Start the server in this instance, so emacs doesn't have to open again

