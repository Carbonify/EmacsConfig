;;Keybindings -------

;; Rebind tab
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;; Reindent buffer
(defun nm-reindent-buffer ()
  "Reindents the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-c i") 'nm-reindent-buffer)


;; Unbind arrow keys, use them for specialized movement instead
(global-set-key (kbd "<right>")    'nm-mark-ring-forward)
(global-set-key (kbd "C-<right>")  'next-buffer)
(global-set-key (kbd "<up>")       'scroll-down-command)
(global-set-key (kbd "C-<left>")   'previous-buffer)
(global-set-key (kbd "<left>")     'pop-to-mark-command)
(global-set-key (kbd "<down>")     'scroll-up-command)

;; Window manipulation
(global-set-key (kbd "S-C-<right>")      'shrink-window-horizontally)
(global-set-key (kbd "S-C-<left>")     'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")      'shrink-window)
(global-set-key (kbd "S-C-<up>")        'enlarge-window)

;; REGISTERS ----------------------
;; Bind home and end to use point register commands
(global-set-key (kbd "<home>") 'jump-to-register)
(global-set-key (kbd "<end>")  'nm-safe-point-to-register)

;; Bind C-home and C-end to use text register commands
(global-set-key (kbd "C-<home>") 'insert-register)
(global-set-key (kbd "C-<end>")  'nm-safe-copy-to-register)

;; Bind meta home and meta end to do macro to register commands
(global-set-key (kbd "M-<home>")   'jump-to-register) ;; Restores the window config at register
(global-set-key (kbd "M-<end>")    'nm-safe-window-config-to-register) ;; Saves the window config to a register

;; Transposing
(global-set-key (kbd "C-t") nil) ;; Remove the old keybinding
(global-set-key (kbd "C-t c") 'transpose-chars)
(global-set-key (kbd "C-t w") 'transpose-words)
(global-set-key (kbd "C-t l") 'transpose-lines)
(global-set-key (kbd "C-t e") 'transpose-sexps)
(global-set-key (kbd "C-t s") 'transpose-sentences)
(global-set-key (kbd "C-t p") 'transpose-paragraphs)

;; End of line and newline
(global-set-key (kbd "C-<return>") 'nm-end-of-line-newline)

;; Finish emacsclient with easier to type bind
(add-hook 'server-switch-hook (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))


;; External function binds, emacs binds
(global-set-key (kbd "C-x C-b")    'ibuffer) ;; Interactive buffer switch
(global-set-key (kbd "C-x s")      'eshell) ;; Open a shell easily in emacs
(global-set-key (kbd "C-c d p")    'nm-delete-in-parentheses) ;; Delete text within parentheses.
(global-set-key (kbd "C-c d q")    'nm-delete-in-quotes) ;; Delete text within quotes.
(global-set-key (kbd "C-c d b")    'nm-delete-in-brackets) ;; Delete text within brackets, eg [],{}, <>
(global-set-key (kbd "C-x C-d")    'nm-insert-date) ;; Insert the date
(global-set-key (kbd "<f8>")       'neotree-toggle)
(global-set-key (kbd "<f5>")       'compile) ;; Quick compile button
(global-set-key (kbd "<f6>")       'gdb) ;; Quick debugger button
(global-set-key (kbd "C-c a")      'align-regexp)
(global-set-key (kbd "C-z")        'repeat) ;;stop accidentally hitting this and minimizing
(global-set-key (kbd "C-c f")      'follow-delete-other-windows-and-split) ;; Enter follow mode quickly
(global-set-key (kbd "C-c n")      'nm-make-temp-file) ;;Create a temporary file that's a bit more persistant than scratch
(global-set-key (kbd "C-c s")      'nm-skim-buffer) ;; Skims a buffer with x lines per x seconds.
(global-set-key (kbd "C-M-s")      'nm-search-all-buffers)
