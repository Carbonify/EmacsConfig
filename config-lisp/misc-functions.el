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
  (when (not (or (derived-mode-p 'text-mode) (derived-mode-p 'special-mode))) ;; If the buffer is a programming one
    (indent-region (point-min) (point-max) nil)) ;; Reindent if editing code, not if text
  (save-excursion (replace-regexp "^\n\\{3,\\}" "\n\n" nil (point-min) (point-max))) ;; Replace more than 2 newlines with 2 newlines
  (untabify (point-min) (point-max))) ;; Turn tabs into spaces

(defun user--delete-in-quotes () "Deletes the text inside of quotes."
  (interactive)
  (search-backward-regexp "[\"\']" (line-beginning-position)) ;; Search for a match on the same line, don't delete across lines
  (forward-char)
  (let  ((lstart (point)))
    (search-forward-regexp "[\"\']" (line-end-position))
    (backward-char)
    (kill-region lstart (point))))

(defun user--delete-in-parentheses () "Deletes the text within parentheses."
  (interactive)
  (search-backward "(" (line-beginning-position)) ;; Search for a match on the same line, don't delete across lines
  (forward-char)
  (let  ((lstart (point)))
    (search-forward ")" (line-end-position))
    (backward-char)
    (kill-region lstart (point))))

(defun user--delete-in-brackets () "Deletes the text within square brackets, angle brackets, and curly brackets."
  (interactive)
  (search-backward-regexp "[[{<]" (line-beginning-position)) ;; Search for a match on the same line, don't delete across lines
  (forward-char)
  (let ((lstart (point)))
    (search-forward-regexp "[]}>]" (line-end-position))
    (backward-char)
    (kill-region lstart (point))))

(defun user--duplicate-start-of-line-or-region () "Duplicates the start of the line if the region is inactive, else duplicates the region."
  (interactive)
  (if mark-active

      (let* ((end (region-end))
             (text (buffer-substring (region-beginning) end)))
        (goto-char end)
        (insert text)
        (push-mark end)
        (setq deactivate-mark nil)
        (exchange-point-and-mark))

    (let ((text (buffer-substring (point) (line-beginning-position))))

      (forward-line 1)
      (if (eobp) ;; If at the end of the buffer
          (newline))
      (push-mark)
      (insert text)
      (open-line 1))))

(provide 'misc-functions)
