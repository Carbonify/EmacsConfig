;; A small plugin that adds mediawiki mode to Emacs, for editing
;; Mediawiki files/articles.

;; Syntax highlighting
(defvar font-mediawiki-sedate-face 'font-mediawiki-sedate-face
  "Face to use for mediawiki  minor keywords.")
(defvar font-mediawiki-italic-face 'font-mediawiki-italic-face
  "Face to use for mediawiki italics.")
(defvar font-mediawiki-bold-face 'font-mediawiki-bold-face
  "Face to use for mediawiki bolds.")
(defvar font-mediawiki-math-face 'font-mediawiki-math-face
  "Face to use for mediawiki math environments.")
(defvar font-mediawiki-string-face 'font-mediawiki-string-face
  "Face to use for strings.")
(defvar font-mediawiki-verbatim-face 'font-mediawiki-verbatim-face
  "Face to use for text in verbatim macros or environments.")
(defvar mediawiki-imenu-generic-expression
  (list '(nil "^==+ *\\(.*[^\n=]\\)==+" 1))
  "Imenu expression for `mediawiki-mode'.  See `imenu-generic-expression'.")


(defface font-mediawiki-bold-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
                    ((assq :weight custom-face-attributes) '(:weight bold))
                    (t '(:bold t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in bold."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-italic-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
                    ((assq :slant custom-face-attributes) '(:slant italic))
                    (t '(:italic t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in italic."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-math-face
  (let ((font (cond ((assq :inherit custom-face-attributes)
                     '(:inherit underline))
                    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-sedate-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark))  (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "DimGray"))
    (((class color) (background dark))  (:foreground "LightGray"))
   ;;;(t (:underline t))
    )
  "Face used to highlight sedate stuff."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-string-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
                    ((assq :slant custom-face-attributes) '(:slant italic))
                    (t '(:italic t)))))
    `((((type tty) (class color))
       (:foreground "green"))
      (((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "RosyBrown"))
      (((class color) (background dark))
       (:foreground "LightSalmon"))
      (t (,@font))))
  "Face used to highlight strings."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-warning-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
                    ((assq :weight custom-face-attributes) '(:weight bold))
                    (t '(:bold t)))))
    `((((class grayscale)(background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale)(background dark))
       (:foreground "LightGray" ,@font))
      (((class color)(background light))
       (:foreground "red" ,@font))
      (((class color)(background dark))
       (:foreground "red" ,@font))
      (t (,@font))))
  "Face for important keywords."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-verbatim-face
  (let ((font (if (and (assq :inherit custom-face-attributes)
                       (if (fboundp 'find-face)
                           (find-face 'fixed-pitch)
                         (facep 'fixed-pitch)))
                  '(:inherit fixed-pitch)
                '(:family "courier"))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown" ,@font))
      (((class color) (background dark))
       (:foreground "burlywood" ,@font))
      (t (,@font))))
  "Face used to highlight TeX verbatim environments."
  :group 'font-mediawiki-highlighting-faces)

(defvar mediawiki-simple-tags
  '("b" "big" "blockquote" "br" "caption" "code" "center" "cite" "del"
    "dfn" "dl" "em" "i" "ins" "kbd" "math" "nowiki" "ol" "pre" "samp"
    "small" "strike" "strong" "sub" "sup" "tt" "u" "ul" "var" "li")
  "Tags that do not accept arguments.")

(defvar mediawiki-complex-tags
  '("a" "div" "font" "table" "td" "th" "tr" "span" "gallery")
  "Tags that accept arguments.")

(defvar mediawiki-url-protocols
  '("ftp" "gopher" "http" "https" "mailto" "news")
  "Valid protocols for URLs in Wikipedia articles.")

(defvar mediawiki-edit-form-vars nil)

(defvar mediawiki-font-lock-keywords
  (list

   ;; Apostrophe-style text markup
   (cons "''''\\([^']\\|[^']'\\)*?\\(''''\\|\n\n\\)"
         'font-lock-builtin-face)
   (cons "'''\\([^']\\|[^']'\\)*?\\('''\\|\n\n\\)"
                                        ;'font-lock-builtin-face)
         'font-mediawiki-bold-face)
   (cons "''\\([^']\\|[^']'\\)*?\\(''\\|\n\n\\)"
         'font-mediawiki-italic-face)

   ;; Headers and dividers
   (list "^\\(==+\\)\\(.*\\)\\(\\1\\)"
         '(1 font-lock-builtin-face)
                                        ;'(2 mediawiki-header-face)
         '(2 font-mediawiki-sedate-face)
         '(3 font-lock-builtin-face))
   (cons "^-----*" 'font-lock-builtin-face)

   ;; Bare URLs and ISBNs
   (cons (concat "\\(^\\| \\)" (regexp-opt mediawiki-url-protocols t)
                 "://[-A-Za-z0-9._\/~%+&#?!=()@]+")
         'font-lock-variable-name-face)
   (cons "\\(^\\| \\)ISBN [-0-9A-Z]+" 'font-lock-variable-name-face)

   ;; Colon indentation, lists, definitions, and tables
   (cons "^\\(:+\\|[*#]+\\||[}-]?\\|{|\\)" 'font-lock-builtin-face)
   (list "^\\(;\\)\\([^:\n]*\\)\\(:?\\)"
         '(1 font-lock-builtin-face)
         '(2 font-lock-keyword-face)
         '(3 font-lock-builtin-face))

   ;; Tags and comments
   (list (concat "\\(</?\\)"
                 (regexp-opt mediawiki-simple-tags t) "\\(>\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-function-name-face t t)
         '(3 font-lock-builtin-face t t))
   (list (concat "\\(</?\\)"
                 (regexp-opt mediawiki-complex-tags t)
                 "\\(\\(?: \\(?:[^\"'/><]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?\\)\\(>\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-function-name-face t t)
         '(3 font-lock-keyword-face t t)
         '(4 font-lock-builtin-face t t))
   (cons (concat "<!-- \\([^->]\\|>\\|-\\([^-]\\|-[^>]\\)\\)*-->")
         '(0 font-lock-comment-face t t))

   ;; External Links
   (list (concat "\\(\\[\\)\\(\\(?:"
                 (regexp-opt mediawiki-url-protocols)
                 "\\)://[-A-Za-z0-9._\/~%-+&#?!=()@]+\\)\\(\\(?: [^]\n]*\\)?\\)\\(\\]\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-variable-name-face t t)
         '(3 font-lock-keyword-face t t)
         '(4 font-lock-builtin-face t t))

   ;; Wiki links
   '("\\(\\[\\[\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t)
     (4 font-lock-keyword-face t t)
     (5 font-lock-builtin-face t t))

   ;; Semantic relations
   '("\\(\\[\\[\\)\\([^]\n|]*\\)\\(::\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t)
     (4 font-lock-constant-face t t)
     (5 font-lock-builtin-face t t)
     (6 font-lock-keyword-face t t)
     (7 font-lock-builtin-face t t))

   ;; Wiki variables
   '("\\({{\\)\\(.+?\\)\\(}}\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t))

   ;; Semantic variables
   '("\\({{{\\)\\(.+?\\)\\(}}}\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t))

   ;; Character entity references
   (cons "&#?[a-zA-Z0-9]+;" '(0 font-lock-type-face t t))

   ;; Preformatted text
   (cons "^ .*$" '(0 font-lock-constant-face t t))

   ;; Math environment (uniform highlight only, no TeX markup)
   (list "<math>\\(\\(\n?.\\)*\\)</math>"
         '(1 font-lock-keyword-face t t))))


;; Functions
(defun mediawiki-create-table () "Creates a mediawiki table."
  (interactive)
  (insert "{| class=\"wikitable\"") (newline)
  (insert "|-") (newline)
  (insert "!Option1!!Option2!!Option3") (newline)
  (insert "|-") (newline)
  (insert "|Value1 || Value2 || Value3") (newline)
  (insert "|-") (newline)
  (insert "|}"))

(defun mediawiki-create-collapse () "Creates the HTML source code for a collapsible text field."
  (interactive)
  (insert "<div class=\"toccolours mw-collapsible mw-collapsed\" style=\"width:800px\">") (newline)
  (insert "Shown content") (newline)
  (insert "<div class=\"mw-collapsible-content\">") (newline)
  (insert "Hidden Content") (newline)
  (insert "</div></div>") (newline))
(defun mediawiki-create-html-tags (string) "Creates HTML tags, given the tag text."
  (interactive "sCreate what tag: ")
  (insert "<" string "></" string ">")
  (search-backward "</"))
(defun mediawiki-surround-region-in-text (textbegin textend begin end) "Surrounds the active region in user-provided text, provide both ends"
  (interactive "sText in front: \nsText on end: \nr")
  (goto-char end)
  (insert textend)
  (goto-char begin)
  (insert textbegin))
(defun mediawiki-surround-region-in-tags (tagname begin end) "Surrounds the active region in user-provided tags"
  (interactive "sWhat tag: \nr")
  (goto-char end)
  (insert "</" tagname ">")
  (goto-char begin)
  (insert "<" tagname ">"))


;; Mode definition ------------------------

;;;###autoload
(define-derived-mode mediawiki-mode text-mode "Mediawiki"
  "Major mode for Mediawiki articles. This mode adds several keybinds to make editing mediawiki articles much easier.
  The standard page commands will work instead on each == header == delimiting pages.
   \\{mediawiki-mode-map}"
  :abbrev-table nil

  ;; Option configs
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'font-lock-defaults) '(mediawiki-font-lock-keywords t nil nil nil))
  (set (make-local-variable 'comment-start-skip) "\\(?:<!\\)?-- *")
  (set (make-local-variable 'comment-end-skip) " *--\\([ \n]*>\\)?")
  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")
  (set (make-local-variable 'paragraph-start) "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")
  (when menu-bar-mode
    (set (make-local-variable 'imenu-generic-expression)
         mediawiki-imenu-generic-expression)
    (imenu-add-to-menubar "Contents"))

  (make-variable-buffer-local 'page-delimiter) ;; Change definition of a page.to be only level 1 headings.
  (setq page-delimiter "^== .* ?==$")

  ;; Key map
  (define-key mediawiki-mode-map (kbd "C-c C-w T") 'mediawiki-create-html-tags)
  (define-key mediawiki-mode-map (kbd "C-c C-w s t") 'mediawiki-surround-region-in-text)
  (define-key mediawiki-mode-map (kbd "C-c C-w s T") 'mediawiki-surround-region-in-tags)
  (define-key mediawiki-mode-map (kbd "C-c C-w t") 'mediawiki-create-table)
  (define-key mediawiki-mode-map (kbd "C-c C-w c") 'mediawiki-create-collapse)) ;;define-derived-mode ends here


(message "Mediawiki mode was loaded." nil)
(provide 'mediawiki-mode)
