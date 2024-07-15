;;; twind.el --- Complete Tailwind classes using the Cheatsheet data -*- lexical-binding: t -*-

(require 'map)

(defgroup twind nil
  "Complete Tailwind classes using the Cheatsheet data."
  :prefix "twind-"
  :group 'convenience)

(defcustom twind-cheatsheet-file
  (locate-user-emacs-file "twind/cheatsheet.json")
  "Path to cheatsheet.json of the Tailwind CSS Cheat Sheet."
  :type 'file)

(defcustom twind-cheatsheet-url
  "https://raw.githubusercontent.com/tailwindcomponents/cheatsheet/main/src/modules/cheatsheet.json"
  "URL of the cheatsheet.json."
  :type 'string)

(defcustom twind-mdn-css-url-prefix
  "https://developer.mozilla.org/en-US/docs/Web/CSS/"
  "URL of the MDN CSS."
  :type 'string)

(defcustom twind-browse-url #'browse-url
  "Function used to browse a documentation resource."
  :type 'function
  :options '(browse-url
             eww))

(defvar twind-cheatsheet-reverse-cache nil)

(defvar-keymap twind-css-mode-map)

;;;###autoload
(define-minor-mode twind-css-mode
  "Minor mode for editing CSS based on a Tailwind theme.")

(defvar-keymap twind-class-mode-map)

;;;###autoload
(define-minor-mode twind-class-mode
  "Minor mode for editing components with Tailwind classes.")

;;;###autoload
(defun twind-insert-class-from-cheatsheet ()
  "Insert Tailwind classes by completing CSS rules."
  (interactive)
  (let ((rules (twind-complete-css-from-cheatsheet "Insert a class for CSS: ")))
    ;; Prepend with a space to avoid collapsing multiple classes.
    (when (looking-back (rx (any word)))
      (insert ?\ ))
    (insert (mapconcat (lambda (rule)
                         (gethash rule twind-cheatsheet-reverse-cache))
                       (cl-remove-duplicates rules :test #'equal)
                       " "))
    ;; Append a space to avoid collapsing multiple classes.
    (when (looking-at (rx (any word)))
      (insert ?\ ))))

;;;###autoload
(defun twind-insert-css-from-cheatsheet ()
  "Insert CSS rules built into Tailwind."
  (interactive)
  (let ((rules (twind-complete-css-from-cheatsheet "Insert CSS: ")))
    (insert (mapconcat (lambda (rule)
                         (concat rule ";"))
                       (cl-remove-duplicates rules :test #'equal)
                       "\n"))))

(defun twind-complete-css-from-cheatsheet (prompt)
  "Complete CSS rules from the cheatsheet."
  (unless twind-cheatsheet-reverse-cache
    (twind--load-cheatsheet))
  (cl-labels
      ((group-function (candidate transform)
         (if transform
             candidate
           (get-text-property 0 'group candidate)))
       (annotator (candidate)
         (concat " " (get-text-property 0 'annotation candidate)))
       (completions (string pred action)
         (if (eq action 'metadata)
             (cons 'metadata
                   (list (cons 'category 'twind-css-rule)
                         (cons 'annotation-function #'annotator)
                         (cons 'group-function #'group-function)))
           (complete-with-action action twind-cheatsheet-reverse-cache string pred))))
    ;; Use semicolons as separator, but avoid replacing multi-rule classes
    (let ((crm-separator "[ \t]*;[^\n]"))
      (completing-read-multiple prompt #'completions))))

(defun twind-download-cheatsheet-json ()
  "Download the latest version of cheatsheet data."
  (interactive)
  (let ((dir (file-name-directory twind-cheatsheet-file)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-current-buffer (url-retrieve-synchronously twind-cheatsheet-url)
    (write-region (1+ url-http-end-of-headers) (point-max) twind-cheatsheet-file)
    (kill-buffer)))

(defun twind--load-cheatsheet ()
  (unless (file-readable-p twind-cheatsheet-file)
    (twind-download-cheatsheet-json))
  (with-temp-buffer
    (insert-file-contents twind-cheatsheet-file)
    (let ((doc (json-parse-buffer :object-type 'alist
                                  :array-type 'list)))
      (if twind-cheatsheet-reverse-cache
          (clrhash twind-cheatsheet-reverse-cache)
        (setq twind-cheatsheet-reverse-cache (make-hash-table :test #'equal :size 5000)))
      (dolist (parent-group doc)
        (let ((parent-group-title (alist-get 'title parent-group)))
          (dolist (subgroup (alist-get 'content parent-group))
            (let* ((subgroup-title (alist-get 'title subgroup))
                   (description (alist-get 'description subgroup))
                   (docs (alist-get 'docs subgroup))
                   ;; (group (format "%s > %s" parent-group-title subgroup-title))
                   (group parent-group-title))
              (cl-flet
                  ((add-rule (class definition)
                     (set-text-properties 0 1 (list 'doc-url docs
                                                    'group group
                                                    'annotation description)
                                          definition)
                     (puthash definition class twind-cheatsheet-reverse-cache)))
                (dolist (rule (alist-get 'table subgroup))
                  (pcase-exhaustive rule
                    (`(,class ,definition)
                     (add-rule class (string-trim-right definition ";")))
                    ((or `("" ,_ ,_)
                         `(,_ ,_ "")))
                    ((and `(,class ,definition ,_)
                          (guard (string-suffix-p ";" definition)))
                     (add-rule class (string-trim-right definition ";")))
                    ;; Ignore colors
                    ((and `(,_ ,_class ,definition)
                          (guard (string-suffix-p ";" definition))))
                    (`("container" ,_ ,_))))))))))))

(defun twind-reference-mdn-css (css-rule)
  "Look up the CSS property on MDN."
  (interactive "sCSS rule: ")
  (let ((property (if (string-match (rx bol (+ (not (any ":")))) css-rule)
                      (match-string 0 css-rule)
                    (error "Invalid CSS rule: %s" css-rule))))
    (funcall twind-browse-url (concat twind-mdn-css-url-prefix property))))

(defun twind-reference-tailwind (css-rule)
  "Look up the documentation on Tailwind CSS."
  (interactive "sCSS rule: ")
  (if-let (url (map-some `(lambda (key _val)
                            (when (equal key ,css-rule)
                              (get-text-property 0 'doc-url key)))
                         twind-cheatsheet-reverse-cache))
      (funcall twind-browse-url url)
    (error "No doc-url property. Maybe not from completion")))

(provide 'twind)
;;; twind.el ends here
