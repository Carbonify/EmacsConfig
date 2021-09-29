;;Keybindings -------

;; Rebind tab
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;; Reindent buffer
(defun nm-reindent-buffer ()
  "Reindents the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-c i") 'nm-reindent-buffer)

;; Window manipulation
(global-set-key (kbd "S-C-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<left>")  'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; REGISTERS ----------------------
(global-set-key (kbd "C-c m")   'nm-safe-point-to-register)
(global-set-key (kbd "C-<end>") 'nm-safe-copy-to-register)
(global-set-key (kbd "C-c w")   'nm-safe-window-config-to-register) ;; Saves the window config to a register


;; Transposing
(global-unset-key (kbd "C-t")) ;; Remove the old keybinding
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
(global-set-key (kbd "C-x C-d")    'digraph-map) ;; Mapping to insert digraphs, if no compose key
(global-set-key (kbd "<f8>")       'neotree-toggle)
(global-set-key (kbd "C-c M-c")    'compile) ;; Quick compile button
(global-set-key (kbd "<f6>")       'gdb-many-windows) ;; Quick debugger button
(global-set-key (kbd "C-c a")      'align-regexp)
(global-set-key (kbd "C-z")        'repeat) ;;stop accidentally hitting this and minimizing
(global-set-key (kbd "C-c f")      'follow-delete-other-windows-and-split) ;; Enter follow mode quickly
(global-set-key (kbd "C-c n")      'nm-make-temp-file) ;;Create a temporary file that's a bit more persistant than scratch
(global-set-key (kbd "C-c s")      'nm-skim-buffer) ;; Skims a buffer with x lines per x seconds.
(global-set-key (kbd "C-M-s")      'nm-search-all-buffers)
