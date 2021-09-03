;; This file holds keyboard macros that are saved by me for future execution.

(fset 'macro-version-to-header-factorio
      (lambda (&optional arg) "Transforms version numbers in the factorio changelog to mediawiki headers."
        (interactive "p") (kmacro-exec-ring-item (quote ([19 13 86 101 114 115 105 111 110 58 return C-backspace 61 61 5 32 61 61] 0 "%d")) arg)))

(fset 'macro-factorio-changelog-headers
      (lambda (&optional arg) "Transforms headers in the factorio changelog into mediawiki headers."
        (interactive "p") (kmacro-exec-ring-item (quote ([19 13 134217840 return 4 32 61 61 61 1 61 61 61 4 14] 0 "%d")) arg)))

(fset 'macro-factorio-forums-link-to-wiki-format
      (lambda (&optional arg) "Changes factorio forum links into (more) links."
        (interactive "p") (kmacro-exec-ring-item (quote ([19 40 104 116 116 112 13 134217826 91 19 13 47 return 19 19 19 13 134217830 32 109 111 101 114 backspace backspace 114 101 93] 0 "%d")) arg)))
