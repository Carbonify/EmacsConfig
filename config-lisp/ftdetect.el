;; A bunch of auto-modes that define how modes should be set based on filetype.

;; Mediawiki
(add-to-list 'auto-mode-alist '("\\.[Mm][Ww]\\'" . mediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.mediawiki\\'" . mediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . mediawiki-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.[Mm][dD]\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

