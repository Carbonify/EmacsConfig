;;Keybindings -------

;; Tab completion
(global-set-key (kbd "<tab>") 'hippie-expand)
(global-set-key (kbd "C-<tab>") 'indent-for-tab-command) ;; Old function of tab

;; Make using ISearch much easier
(define-key isearch-mode-map [next]     'isearch-repeat-forward)
(define-key isearch-mode-map [prior]    'isearch-repeat-backward)
(global-set-key (kbd "C-s")             'isearch-forward-regexp)
(global-set-key (kbd "C-r")             'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")           'multi-isearch-buffers-regexp)
(global-unset-key (kbd "C-M-r"))

;; Search all loaded buffers for a regex
(global-set-key [f7] 'user--search-all-buffers)

;; Unbind arrow keys, use them for specialized movement instead
(global-set-key (kbd "<right>")    'user--mark-ring-forward)
(global-set-key (kbd "C-<right>")  'next-buffer)
(global-set-key (kbd "<up>")       'scroll-down-command)
(global-set-key (kbd "C-<left>")   'previous-buffer)
(global-set-key (kbd "<left>")     'pop-to-mark-command)
(global-set-key (kbd "<down>")     'scroll-up-command)

;; REGISTERS ----------------------
;; Bind home and end to use point register commands
(global-set-key (kbd "<home>") 'jump-to-register)
(global-set-key (kbd "<end>") 'user--safe-point-to-register)

;; Bind C-home and C-end to use text register commands
(global-set-key (kbd "C-<home>") 'insert-register)
(global-set-key (kbd "C-<end>") 'user--safe-copy-to-register)

;; Bind meta home and meta end to do macro to register commands
(global-set-key (kbd "M-<home>") 'jump-to-register) ;; Restores the window config at register
(global-set-key (kbd "M-<end>") 'user--safe-window-config-to-register) ;; Saves the window config to a register


;; Disable moving point with clicks
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))


;; Transposing
(global-set-key (kbd "C-t") nil) ;; Remove the old keybinding
(global-set-key (kbd "C-t c") 'transpose-chars)
(global-set-key (kbd "C-t w") 'transpose-words)
(global-set-key (kbd "C-t l") 'transpose-lines)
(global-set-key (kbd "C-t e") 'transpose-sexps)
(global-set-key (kbd "C-t s") 'transpose-sentences)
(global-set-key (kbd "C-t p") 'transpose-paragraphs)

;; End of line and newline
(global-set-key (kbd "C-<return>") 'user--end-of-line-newline)

;; External function binds, emacs binds
(global-set-key (kbd "C-x C-b")    'ibuffer) ;; Interactive buffer switch
(global-set-key (kbd "C-c s")      'eshell) ;; Open a shell easily in emacs
(global-set-key (kbd "C-c d p")    'user--delete-in-parentheses) ;; Delete text within parentheses.
(global-set-key (kbd "C-c d q")    'user--delete-in-quotes) ;; Delete text within quotes.
(global-set-key (kbd "C-c d b")    'user--delete-in-brackets) ;; Delete text within brackets, eg [],{}, <>
(global-set-key (kbd "C-x C-d")    'user--insert-date) ;; Insert the date
(global-set-key (kbd "<f8>")       'neotree-toggle)
(global-set-key (kbd "C-c a")      'align-regexp)
(global-set-key (kbd "C-z")        'repeat) ;;stop accidentally hitting this and minimizing
(global-set-key (kbd "C-c f")      'follow-delete-other-windows-and-split) ;; Enter follow mode quickly
(global-set-key (kbd "C-c n")      'user--make-temp-file) ;;Create a temporary file that's a bit more persistant than scratch
