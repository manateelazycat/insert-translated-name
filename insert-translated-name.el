;;; insert-translated-name.el --- Insert translated string as variable or function name

;; Filename: insert-translated-name.el
;; Description: Insert translated string as variable or function name
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-22 10:54:16
;; Version: 0.1
;; Last-Updated: 2018-09-22 10:54:16
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/insert-translated-name.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Insert translated string as variable or function name
;;

;;; Installation:
;;
;; Put insert-translated-name.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'insert-translated-name)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET insert-translated-name RET
;;

;;; Change log:
;;
;; 2018/09/22
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(defconst insert-translated-name-api-url
  "http://fanyi.youdao.com/openapi.do?keyfrom=YouDaoCV&key=659600698&type=data&doctype=json&version=1.1&q=%s"
  "Youdao dictionary API template, URL `http://dict.youdao.com/'.")

(defun insert-translated-name (word)
  (interactive "sTranslate with current mode style: ")
  (cond ((derived-mode-p 'emacs-lisp-mode)
         (insert-translated-name-with-line word))
        ((derived-mode-p 'web-mode)
         (insert-translated-name-with-camel word))
        ((derived-mode-p 'ruby-mode)
         (insert-translated-name-with-underline word))
        (t
         (insert-translated-name-with-underline word))))

(defun insert-translated-name-with-line (word)
  (interactive "sTranslate with line style: ")
  (let* ((translation (insert-translated-name-get-translation word))
         (words (split-string translation " ")))
    (insert (string-join (mapcar 'downcase words) "-"))))

(defun insert-translated-name-with-underline (word)
  (interactive "sTranslate with underline style: ")
  (let* ((translation (insert-translated-name-get-translation word))
         (words (split-string translation " ")))
    (insert (string-join (mapcar 'downcase words) "_"))))

(defun insert-translated-name-with-camel (word)
  (interactive "sTranslate with camel style: ")
  (let* ((translation (insert-translated-name-get-translation word))
         (words (split-string translation " ")))
    (insert (concat (downcase (car words)) (string-join (mapcar 'capitalize (cdr words)))))))

(defun insert-translated-name-get-translation (word)
  "Format request result of WORD."
  (elt (assoc-default 'translation (insert-translated-name-request word)) 0))

(defun insert-translated-name-request (word)
  "Request WORD, return JSON as an alist if successes."
  (let (json)
    (with-current-buffer (url-retrieve-synchronously
                          (format insert-translated-name-api-url (url-hexify-string word)))
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (setq json (json-read-from-string
                  (buffer-substring-no-properties (point) (point-max))))
      (kill-buffer (current-buffer)))
    json))

(provide 'insert-translated-name)

;;; insert-translated-name.el ends here
