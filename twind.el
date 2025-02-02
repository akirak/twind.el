;;; twind.el --- Complete Tailwind classes using the Cheatsheet data -*- lexical-binding: t -*-

;; Copyright (C) 2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;; URL: https://github.com/akirak/twind.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Twind lets you insert Tailwind classes from rules defined in the Tailwind CSS
;; Cheatsheet.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'skeleton)

(defvar url-http-end-of-headers)
(defvar crm-separator)

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

(defcustom twind-extra-class-alist
  ;; Based on https://tailwindcss.com/docs/hover-focus-and-other-states#quick-reference
  ;; TODO: Automate updating of this definition
  (eval-when-compile
    (mapcar (lambda (ent)
              (append ent
                      '(doc-url "https://tailwindcss.com/docs/hover-focus-and-other-states")))
            '(("hover:" "@media (hover: hover) { &:hover }")
              ("focus:" "&:focus")
              ("focus-within:" "&:focus-within")
              ("focus-visible:" "&:focus-visible")
              ("active:" "&:active")
              ("visited:" "&:visited")
              ("target:" "&:target")
              ("*:" ":is(& > *)")
              ("**:" ":is(& *)")
              ("has-[...]:" "&:has(...)")
              ("group-[...]:" "&:is(:where(.group)... *)")
              ("peer-[...]:" "&:is(:where(.peer)... ~ *)")
              ("in-[...]:" ":where(...) &")
              ("not-[...]:" "&:not(...)")
              ("inert:" "&:is([inert], [inert] *)")
              ("first:" "&:first-child")
              ("last:" "&:last-child")
              ("only:" "&:only-child")
              ("odd:" "&:nth-child(odd)")
              ("even:" "&:nth-child(even)")
              ("first-of-type:" "&:first-of-type")
              ("last-of-type:" "&:last-of-type")
              ("only-of-type:" "&:only-of-type")
              ("nth-[...]:" "&:nth-child(...)")
              ("nth-last-[...]:" "&:nth-last-child(...)")
              ("nth-of-type-[...]:" "&:nth-of-type(...)")
              ("nth-last-of-type-[...]:" "&:nth-last-of-type(...)")
              ("empty:" "&:empty")
              ("disabled:" "&:disabled")
              ("enabled:" "&:enabled")
              ("checked:" "&:checked")
              ("indeterminate:" "&:indeterminate")
              ("default:" "&:default")
              ("optional:" "&:optional")
              ("required:" "&:required")
              ("valid:" "&:valid")
              ("invalid:" "&:invalid")
              ("in-range:" "&:in-range")
              ("out-of-range:" "&:out-of-range")
              ("placeholder-shown:" "&:placeholder-shown")
              ("autofill:" "&:autofill")
              ("read-only:" "&:read-only")
              ("before:" "&::before")
              ("after:" "&::after")
              ("first-letter:" "&::first-letter")
              ("first-line:" "&::first-line")
              ("marker:" "&::marker, & *::marker")
              ("selection:" "&::selection")
              ("file:" "&::file-selector-button")
              ("backdrop:" "&::backdrop")
              ("placeholder:" "&::placeholder")
              ("sm:" "@media (width >= 40rem)")
              ("md:" "@media (width >= 48rem)")
              ("lg:" "@media (width >= 64rem)")
              ("xl:" "@media (width >= 80rem)")
              ("2xl:" "@media (width >= 96rem)")
              ("min-[...]:" "@media (width >= ...)")
              ("max-sm:" "@media (width < 40rem)")
              ("max-md:" "@media (width < 48rem)")
              ("max-lg:" "@media (width < 64rem)")
              ("max-xl:" "@media (width < 80rem)")
              ("max-2xl:" "@media (width < 96rem)")
              ("max-[...]:" "@media (width < ...)")
              ("@3xs:" "@container (width >= 16rem)")
              ("@2xs:" "@container (width >= 18rem)")
              ("@xs:" "@container (width >= 20rem)")
              ("@sm:" "@container (width >= 24rem)")
              ("@md:" "@container (width >= 28rem)")
              ("@lg:" "@container (width >= 32rem)")
              ("@xl:" "@container (width >= 36rem)")
              ("@2xl:" "@container (width >= 42rem)")
              ("@3xl:" "@container (width >= 48rem)")
              ("@4xl:" "@container (width >= 56rem)")
              ("@5xl:" "@container (width >= 64rem)")
              ("@6xl:" "@container (width >= 72rem)")
              ("@7xl:" "@container (width >= 80rem)")
              ("@min-[...]:" "@container (width >= ...)")
              ("@max-3xs:" "@container (width < 16rem)")
              ("@max-2xs:" "@container (width < 18rem)")
              ("@max-xs:" "@container (width < 20rem)")
              ("@max-sm:" "@container (width < 24rem)")
              ("@max-md:" "@container (width < 28rem)")
              ("@max-lg:" "@container (width < 32rem)")
              ("@max-xl:" "@container (width < 36rem)")
              ("@max-2xl:" "@container (width < 42rem)")
              ("@max-3xl:" "@container (width < 48rem)")
              ("@max-4xl:" "@container (width < 56rem)")
              ("@max-5xl:" "@container (width < 64rem)")
              ("@max-6xl:" "@container (width < 72rem)")
              ("@max-7xl:" "@container (width < 80rem)")
              ("@max-[...]:" "@container (width < ...)")
              ("dark:" "@media (prefers-color-scheme: dark)")
              ("portrait:" "@media (orientation: portrait)")
              ("landscape:" "@media (orientation: landscape)")
              ("motion-safe:" "@media (prefers-reduced-motion: no-preference)")
              ("motion-reduce:" "@media (prefers-reduced-motion: reduce)")
              ("contrast-more:" "@media (prefers-contrast: more)")
              ("contrast-less:" "@media (prefers-contrast: less)")
              ("print:" "@media print")
              ("supports-[...]:" "@supports (…)")
              ("aria-busy:" "&[aria-busy=\"true\"]")
              ("aria-checked:" "&[aria-checked=\"true\"]")
              ("aria-disabled:" "&[aria-disabled=\"true\"]")
              ("aria-expanded:" "&[aria-expanded=\"true\"]")
              ("aria-hidden:" "&[aria-hidden=\"true\"]")
              ("aria-pressed:" "&[aria-pressed=\"true\"]")
              ("aria-readonly:" "&[aria-readonly=\"true\"]")
              ("aria-required:" "&[aria-required=\"true\"]")
              ("aria-selected:" "&[aria-selected=\"true\"]")
              ("aria-[...]:" "&[aria-…]")
              ("data-[...]:" "&[data-...]")
              ("rtl:" "[dir=\"rtl\"] &")
              ("ltr:" "[dir=\"ltr\"] &")
              ("open:" "&:is([open], :popover-open)")
              ("forced-colors:" "@media (forced-colors: active)")
              ("starting:" "@starting-style"))))
  "List of extra static entries for completion.

Each entry should take the form (CLASS DEFINITION . PLIST) where PLIST
is a list of properties for completion. The list of supported properties are:

  * doc-url: The documentation URL.

  * description: A description of the definition.

This is used for completing a Tailwind class from its definition, but
not the other way around. It is mostly intended for supporting pseudo
classes of Tailwind. See
<https://tailwindcss.com/docs/hover-focus-and-other-states#pseudo-classes>
for their definitions.

Please ensure pseudo classes are suffixed with a colon (\":\").

If three consecutive dots (\"...\") are included in the class, it will
be replaced with a user input using skeleton."
  :type '(alist :key-type string
                :value-type string))

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
  (let ((rules (twind-complete-css-from-cheatsheet "Insert the class for CSS: ")))
    ;; Prepend with a space to avoid collapsing multiple classes.
    (when (and (not (twind--pseudo-class-p (car rules)))
               (looking-back (rx (any word)) (line-beginning-position)))
      (insert ?\ ))
    (let ((skeleton-end-newline nil))
      (skeleton-insert (cons `nil
                             (mapcan (lambda (rule)
                                       (let ((class (gethash rule twind-cheatsheet-reverse-cache)))
                                         (append (if (string-match (regexp-quote "...") class)
                                                     (list (substring class 0 (match-beginning 0))
                                                           `(read-string
                                                             ,(concat
                                                               "Format the inner content of "
                                                               rule ": "))
                                                           (substring class (match-end 0)))
                                                   (list class))
                                                 (unless (twind--pseudo-class-p class)
                                                   '(" ")))))
                                     (cl-remove-duplicates rules :test #'equal)))))))

(defun twind--pseudo-class-p (class)
  (string-suffix-p ":" class))

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
            (let* ((description (alist-get 'description subgroup))
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
                    (`("container" ,_ ,_)))))))))
      (cl-flet
          ((add-rule (class definition properties)
             (set-text-properties 0 1 properties definition)
             (puthash definition class twind-cheatsheet-reverse-cache)))
        (dolist (rule twind-extra-class-alist)
          (pcase-exhaustive rule
            (`(,class ,definition . ,plist)
             (add-rule class definition (append plist '(group "extras"))))))))))

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
