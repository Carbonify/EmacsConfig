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

(defun user--rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file name new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun user--insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))


(defun user--generate-numbered-list (start end)
  "Creates a numbered list from provided start to provided end."
  (interactive "nStart num:\nnEnd num:")
  (let ((x  start))
    (while (<= x end)
      (insert (number-to-string x) ".")
      (newline)
      (setq x (+ x 1)))))

(defun user--mark-ring-forward ()
  "Moves forward through the mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (message "No marks set." nil))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defun user--make-temp-file (name)
  "Creates a temporary file in the system temp directory, for various purposes."
  (interactive "sFile name:")
  (generate-new-buffer name)
  (switch-to-buffer name)
  (write-file (concat temporary-file-directory name)))

(defun user--search-all-buffers (regexp) "Search all open buffers for a regex. Open an occur-like window."
  (interactive "sRegexp: ")
  (multi-occur-in-matching-buffers "." regexp t))

(defun user--safe-point-to-register (register)
  "Asks for confirmation before overwriting an existing register with a point-to-register."
  (interactive "cRegister:")
  (if (not (get-register register))
      (point-to-register register)
    (if (y-or-n-p "Replace existing register?")
        (point-to-register register))))

(defun user--safe-copy-to-register (register)
  "Asks for confirmation before overwriting an existing register with a copy-to-register."
  (interactive "cRegister:")
    (if (not (get-register register))
      (copy-to-register register (region-beginning) (region-end))
    (if (y-or-n-p "Replace existing register?")
        (copy-to-register register (region-beginning) (region-end)))))

(defun user--safe-window-config-to-register (register)
  "Asks for confirmation before overwriting an existing register with a window-configuration-to-register."
  (interactive "cRegister:")
    (if (not (get-register register))
      (window-configuration-to-register register)
    (if (y-or-n-p "Replace existing register?")
        (window-configuration-to-register register))))

(defun user--end-of-line-newline ()
  "Moves to the end of the line and newlines."
  (interactive)
  (end-of-line)
  (newline))

(defun user--skim-buffer (lines seconds)
  "Scrolls the buffer `lines` every `seconds`, which allows for roughly skimming over a buffer."
  (interactive "nScroll lines: \nnEvery seconds: ")
  (while (not (= (point) (point-max)))
    (scroll-up-line lines)
    (end-of-line)
    (sit-for seconds)))
