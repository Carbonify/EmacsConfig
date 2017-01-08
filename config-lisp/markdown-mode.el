;; Plugin to add markdown support to Emacs.

;;;###autoload
(define-derived-mode markdown-mode text-mode "Markdown"
  "Major mode for markdown documents.
  \\{markdown-mode-map}"
  :abbrev-table nil
  (defun surround-region-in-text (text) "Surrounds the active region in user-provided text."
    (interactive "sWhat text: ")
    (insert text)
    (exchange-point-and-mark)
    (insert text)
    (exchange-point-and-mark))
  (defun embolden-region-text () "Makes the selected region bold, with markdown."
    (interactive)
    (insert "**")
    (exchange-point-and-mark)
    (insert "**")
    (exchange-point-and-mark))
  (defun italisize-region-text () "Makes the selected region italic, with markdown"
    (interactive)
    (insert "*")
    (exchange-point-and-mark)
    (insert "*")
    (exchange-point-and-mark))
  
  (define-key markdown-mode-map (kbd "C-c s t") 'surround-region-in-text)
  (define-key markdown-mode-map (kbd "C-c b") 'embolden-region-text)
  (define-key markdown-mode-map (kbd "C-c i") 'italisize-region-text)) ;; Define-derived-mode ends here.

(message "Markdown mode was loaded." nil)
(provide 'markdown-mode)
