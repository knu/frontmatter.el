;;; frontmatter.el --- Functions for manipulating YAML frontmatter data

;; Copyright (c) 2023 Akinori MUSHA
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/frontmatter.el
;; Created: 6 Nov 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools

;;; Commentary:
;;
;; This package provides functions for manipulating YAML frontmatter data.
;;
;; Usage:
;;
;; * `frontmatter-add-date'
;;
;;   This command adds the "date" frontmatter property to the current buffer.
;;
;;     (define-key markdown-mode-map (kbd "C-c d") #'frontmatter-add-date)
;;     (define-key gfm-mode-map      (kbd "C-c d") #'frontmatter-add-date)
;;
;; * `frontmatter-update-timestamps'
;;
;;   This command can be used to mimic what the "Update time on edit"
;;   plugin (https://github.com/beaussan/update-time-on-edit-obsidian)
;;   does like so:
;;
;;     (require 'obsidian)
;;
;;     (defun frontmatter-update-timestamps-before-save ()
;;       (add-hook 'before-save-hook #'frontmatter-update-timestamps nil t))
;;     (add-hook 'obsidian-mode-hook #'frontmatter-update-timestamps-before-save)

;;; Code:

(require 'rx)
(require 'yaml)

(defgroup frontmatter nil
  "Functions for manipulating YAML frontmatter data."
  :group 'text)

(defcustom frontmatter-date-format "%F"
  "Time format used by `frontmatter-add-date'.

See `format-time-string' for details."
  :type 'string
  :group 'frontmatter)

(defcustom frontmatter-date-time-zone nil
  "Time zone used by `frontmatter-add-date'.

See `format-time-string' for details."
  :type '(choice (const :tag "Local Time" nil)
                 (const :tag "UTC" t)
                 (string :tag "TZ string")
                 (integer :tag "UTC offset in seconds")
                 (sexp :tag "Other expression"))
  :group 'frontmatter)

(defcustom frontmatter-timestamp-format "%FT%H:%M"
  "Time format used by `frontmatter-update-timestamps'.

See `format-time-string' for details."
  :type 'string
  :group 'frontmatter)

(defcustom frontmatter-timestamp-time-zone nil
  "Time zone used by `frontmatter-update-timestamps'.

See `format-time-string' for details."
  :type '(choice (const :tag "Local Time" nil)
                 (const :tag "UTC" t)
                 (string :tag "TZ string")
                 (integer :tag "UTC offset in seconds")
                 (sexp :tag "Other expression"))
  :group 'frontmatter)

(defvar frontmatter-section-regexp
  (rx bos
      "---\n"
      (group
        (*? (* not-newline) "\n"))
      (| "---" "...") "\n")
  "Regular expression to match a frontmatter section.

Group 1 should match the YAML document part between delimiters.")

(defvar frontmatter-timestamp-regexp
 (rx (| (: (repeat 4 digit) "-" digit digit "-" digit digit) ;; ymd
        (: (repeat 4 digit) "-" digit (? digit) "-" digit (? digit)
           (| (any "Tt") (+ (any " \t")))
           digit (? digit)
           ":" digit digit
           (? ":" digit digit
              (? "." (* digit)))
           (? (| (: (* (any " \t")) "Z")
                 (: (any "-+") digit (? digit) (? ":" digit digit)))))))
 "Regular expression to match a timestamp in a frontmatter property value.")

(defvar frontmatter-quoted-timestamp-property-regexp
  (rx bol (group (+ wordchar) ": ")
      ?\" (group (regexp frontmatter-timestamp-regexp)) ?\" eol)
 "Regular expression to match a frontmatter property with a quoted timestamp value.")

(defun frontmatter-parse (string)
  "Parse a YAML STRING as a frontmatter document."
  (yaml-parse-string string :object-type 'alist))

(defun frontmatter-encode (document)
  "Encode a frontmatter DOCUMENT to a string."
  (replace-regexp-in-string
   frontmatter-quoted-timestamp-property-regexp
   "\\1\\2"
   (yaml-encode document)))

(defun frontmatter-body-region ()
  "Get the frontmatter document region of the current buffer as a cons cell.

Return nil if there is none."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at frontmatter-section-regexp)
        (cons (match-beginning 1) (match-end 1)))))

(defun frontmatter-extract ()
  "Extract a parsed frontmatter document from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (pcase (frontmatter-body-region)
      (`(,beg . ,end)
       (frontmatter-parse (buffer-substring-no-properties beg end))))))

(defun frontmatter-embed (document)
  "Embed DOCUMENT as a frontmatter document in the current buffer."
  (let ((yaml (frontmatter-encode document)))
    (pcase (frontmatter-body-region)
      (`(,beg . ,end)
       (save-excursion
         (goto-char beg)
         (delete-region beg end)
         (insert yaml "\n")
         document))
      (`nil
       (save-excursion
         (goto-char (point-min))
         (insert "---\n" yaml "\n---\n")
         document)))))

;;;###autoload
(defun frontmatter-update (func)
  "Update the frontmatter document of the current buffer using FUNC.

It is passed the current frontmatter document as an alist and should
return an updated document.  If it returns nil, the frontmatter
section will get removed."
  (let* ((document (frontmatter-extract))
         (document (funcall func document)))
    (frontmatter-embed document)))

;;;###autoload
(defun frontmatter-get (key)
  "Get a frontmatter property value associated with KEY."
  (alist-get key (frontmatter-extract)))

;;;###autoload
(defun frontmatter-set (key value)
  "Set a frontmatter property value associated with KEY to VALUE."
  (frontmatter-update
   (lambda (document)
     (setf (alist-get key document) value)
     document)))

;;;###autoload
(defun frontmatter-add-date ()
  "Add the `date' frontmatter property to the current buffer if nonexistent.

Customize the format and time zone via
`frontmatter-date-format' and
`frontmatter-date-time-zone'."
  (interactive)
  (or
   (frontmatter-get 'date)
   (frontmatter-set 'date (format-time-string frontmatter-date-format frontmatter-date-time-zone))))

;;;###autoload
(defun frontmatter-update-timestamps ()
  "Set or update the timestamp frontmatter properties of the current buffer.

The value of the `created' property is set to the current time
only when it does not exist.  The value of the `updated' property
is added if it does not exist, and set to the current time.

Customize the format and time zone via
`frontmatter-timestamp-format' and
`frontmatter-timestamp-time-zone'."
  (interactive)
  (let ((timestring (format-time-string frontmatter-timestamp-format frontmatter-timestamp-time-zone)))
    (frontmatter-set 'updated timestring)
    (or
     (frontmatter-get 'created)
     (frontmatter-set 'created timestring))))

(provide 'frontmatter)
;;; frontmatter.el ends here
