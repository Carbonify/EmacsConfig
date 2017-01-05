;; A small plugin that adds mediawiki mode to Emacs, for editing
;; Mediawiki files/articles.

(define-derived-mode mediawiki-mode text-mode "Mediawiki"
  "Major mode for Mediawiki articles. This mode adds several keybinds to make editing mediawiki articles much easier.
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
  (defun create-html-tags (string) "Creates HTML tags, given the tag text."
    (interactive "sCreate what tag: ")
    (insert "<" string "></" string ">"))
  (defun surround-region-in-text (text) "Surrounds the active region in user-provided text."
    (interactive "sWhat text: ")
    (insert text)
    (exchange-point-and-mark)
    (insert text)
    (exchange-point-and-mark))
  (defun surround-region-in-tags (tagname begin end) "Surrounds the active region in user-provided tags"
    (interactive "sWhat tag: \nr")
    (message "Begin: %s" begin)
    (message "%s" end)
    (goto-char end)
    (insert "</" tagname ">")
    (goto-char begin)
    (insert "<" tagname ">"))

  (define-key mediawiki-mode-map (kbd "C-c C-w T") 'create-html-tags)
  (define-key mediawiki-mode-map (kbd "C-c C-w s t") 'surround-region-in-text)
  (define-key mediawiki-mode-map (kbd "C-c C-w s T") 'surround-region-in-tags)
  (define-key mediawiki-mode-map (kbd "C-c C-w t") 'create-mediawiki-table)
  (define-key mediawiki-mode-map (kbd "C-c C-w c") 'create-mediawiki-collapse)) ;;define-derived-mode ends here

(add-to-list 'auto-mode-alist '("\\.mw\\'" . mediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.mediawiki\\'" . mediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . mediawiki-mode))

(provide 'mediawiki-mode)
