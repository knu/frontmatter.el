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

;;; Code:

(require 'rx)
(require 'yaml)

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
           ":" digit digit
           (? "." (* digit))
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
 
(defun frontmatter-update (func)
  "Update the frontmatter document of the current buffer using FUNC.

It is passed the current frontmatter document as an alist and should
return an updated document.  If it returns nil, the frontmatter
section will get removed."
  (let* ((document (frontmatter-extract))
         (document (funcall func document)))
    (frontmatter-embed document)))

(defun frontmatter-get (key)
  "Get a frontmatter property value associated with KEY."
  (alist-get key (frontmatter-extract)))

(defun frontmatter-set (key value)
  "Set a frontmatter property value associated with KEY to VALUE."
  (frontmatter-update
   (lambda (document)
     (setf (alist-get key document) value)
     document)))

(provide 'frontmatter)
;;; frontmatter.el ends here
