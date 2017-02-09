;; A simple file to hold misc functions of my config, to stop them from consuming space in my init file.

(defun user--save-macro (name)
  "Save a macro. Take a name as an argument and save the last defined macro under this name."
  (interactive "SName of the macro :")
  (kmacro-name-last-macro name)
  (find-file "~/.emacs.d/macros.el")
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  (save-buffer)
  (switch-to-buffer nil))

(defun user--clean-buffer () "Cleans the buffer by re-indenting, changing tabs to spaces, and removing trailing whitespace."
  (interactive)
  (delete-trailing-whitespace) ;; Remove whitespace from the ends of lines
  (when (derived-mode-p 'prog-mode)
    (indent-region (point-min) (point-max) nil)) ;; Reindent if editing code, not if text
  (save-excursion (replace-regexp "^\n\\{3,\\}" "\n\n" nil (point-min) (point-max))) ;; Replace more than 2 newlines with 2 newlines
  (untabify (point-min) (point-max))) ;; Turn tabs into spaces

(defun user--delete-in-quotes () "Deletes the text inside of parentheses."
  (interactive)
  (setq start-loc (point)) ;;Save the location the command started from.
  (setq lstart-line (line-number-at-pos (point)))
  (search-backward "\"" (line-beginning-position))
  (forward-char)
  (setq lstart (point))
  (search-forward "\"" (line-end-position))
  (backward-char)
  (kill-region lstart (point)))

(defun user--delete-in-parentheses () "Deletes the text within quotes."
  (interactive)
  (setq start-loc (point)) ;;Save the location the command started from.
  (setq lstart-line (line-number-at-pos (point)))
  (search-backward "(" (line-beginning-position))
  (forward-char)
  (setq lstart (point))
  (search-forward ")" (line-end-position))
  (backward-char)
  (kill-region lstart (point)))


(provide 'misc-functions)
