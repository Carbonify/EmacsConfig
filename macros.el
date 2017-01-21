;; This file holds keyboard macros that are saved by me for future execution.

(fset 'macro-capitalize-start-of-line
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\343" 0 "%d")) arg)))
