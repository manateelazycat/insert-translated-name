;;; insert-translated-name.el --- Insert translated string as variable or function name  -*- lexical-binding: t; -*-

;; Filename: insert-translated-name.el
;; Description: Insert translated string as variable or function name
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-22 10:54:16
;; Version: 3.0
;; Last-Updated: 2022-10-11 22:48:58
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/insert-translated-name.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `json' `subr-x'
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
;; `insert-translated-name-line-style-mode-list'
;; `insert-translated-name-underline-style-mode-list'
;; `insert-translated-name-camel-style-mode-list'
;; `insert-translated-name-font-lock-mark-word'
;;

;;; Change log:
;;
;; 2023/07/02
;;      * Remove `deno-bridge' dependence.
;;
;; 2022/10/11
;;      * Use `deno-bridge' fetch translation.
;;
;; 2020/03/22
;;      * Use `default-input-method' instead pyim method.
;;
;; 2019/03/19
;;      * Add css-mode in line style.
;;
;; 2019/03/16
;;      * Don't print notify message if current cursor in minibuffer.
;;
;; 2019/02/20
;;      * Add go-mode in `insert-translated-name-camel-style-mode-list'.
;;
;; 2019/01/29
;;      * Add `inferior-emacs-lisp-mode' in `insert-translated-name-line-style-mode-list'.
;;
;; 2018/12/09
;;      * Fix bug of `insert-translated-name-in-string-p' when cursor at left side of string.
;;
;; 2018/12/07
;;      * Add `json' and `subr-x' depend.
;;
;; 2018/12/02
;;      * Use `get-text-property' improve algorithm of `insert-translated-name-in-string-p' and `insert-translated-name-in-commit-p'
;;
;; 2018/12/01
;;      * Add `insert-translated-name-origin-style-mode-list'.
;;
;; 2018/11/18
;;      * Refacotry to remove duplicate variable.
;;
;; 2018/11/12
;;      * Remove Mac color, use hex color instead.
;;
;; 2018/11/12
;;      * Remove the function of continuous translation, it is not easy to use.
;;
;; 2018/09/26
;;      * Add `insert-translated-name-use-original-translation'.
;;      * Nothing happen if input word is empty.
;;      * Make `insert-translated-name-insert' support prefix arg.
;;
;; 2018/09/25
;;      * Add `insert-translated-name-in-commit-buffer-p' option to make english assistants available in magit.
;;      * Make english assistants available in minibuffer.
;;
;; 2018/09/24
;;      * Add option `insert-translated-name-translate-engine' and default use Google.
;;      * Support pyim now.
;;
;; 2018/09/23
;;      * Store placeholder in buffer local's hash instead insert placeholder uuid in buffer.
;;      * Make `insert-translated-name-replace-*' functions support region.
;;      * Call `insert-translated-name-insert-original-translation' when cursor in comment or string.
;;
;; 2018/09/22
;;      * First released.
;;      * Change query translation asynchronous, don't insert buffer if query duration more than 2 seconds.
;;      * Use overlay as input way, and add `insert-translated-name-replace-*' functions.
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
(require 'json)
(require 'subr-x)
(require 'cl-seq)

;;; Code:

;;;;;;;;;;;;;;;;;;;;; Customize options ;;;;;;;;;;;;;;;;;;;;;
(defgroup insert-translated-name nil
  "Search and refacotry code base on ripgrep."
  :group 'insert-translated-name)

(defcustom insert-translated-name-program "crow"
  "Use `crow' or `ollama' to translate input."
  :group 'insert-translated-name
  :type 'string)

(defcustom insert-translated-name-crow-engine "google"
  "the crow app engine"
  :group 'insert-translated-name
  :type 'string)

(defcustom insert-translated-name-ollama-model-name "zephyr"
  "The model name of ollama."
  :group 'insert-translated-name
  :type 'string)

(defface insert-translated-name-font-lock-mark-word
  '((t (:foreground "White" :background "#007aff" :bold t)))
  "Face for keyword match."
  :group 'insert-translated-name)

(defvar insert-translated-name-ollama-file (expand-file-name "ollama.py" (if load-file-name
                                                                             (file-name-directory load-file-name)
                                                                           default-directory)))

(defvar insert-translated-name-origin-style-mode-list
  '(text-mode erc-mode rcirc-mode))

(defvar insert-translated-name-line-style-mode-list
  '(web-mode emacs-lisp-mode inferior-emacs-lisp-mode css-mode))

(defvar insert-translated-name-camel-style-mode-list
  '(js-mode go-mode))

(defvar insert-translated-name-underline-style-mode-list
  '(ruby-mode))

(defvar insert-translated-name-default-style "underline"
  "The default translation style, which can be set to \"origin\", \"line\", \"camel\" or \"underline\".")
;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;
(defun insert-translated-name-insert (arg)
  (interactive "p")
  (if (or
       (equal arg 4)
       (and (boundp 'insert-translated-name-original-translation)
            insert-translated-name-original-translation)
       (insert-translated-name-in-string-p)
       (insert-translated-name-in-comment-p)
       (insert-translated-name-in-commit-buffer-p)
       (minibuffer-window-active-p (get-buffer-window)))
      (insert-translated-name-insert-original-translation)
    (insert-translated-name-active
     (cond
      ((insert-translated-name-match-modes insert-translated-name-origin-style-mode-list)
       "origin")
      ((insert-translated-name-match-modes insert-translated-name-line-style-mode-list)
       "line")
      ((insert-translated-name-match-modes insert-translated-name-camel-style-mode-list)
       "camel")
      ((insert-translated-name-match-modes insert-translated-name-underline-style-mode-list)
       "underline")
      (t
       insert-translated-name-default-style)))))

(defun insert-translated-name-insert-original-translation ()
  (interactive)
  (insert-translated-name-active "comment"))

(defun insert-translated-name-insert-with-line ()
  (interactive)
  (insert-translated-name-active "line"))

(defun insert-translated-name-insert-with-underline ()
  (interactive)
  (insert-translated-name-active "underline"))

(defun insert-translated-name-insert-with-camel ()
  (interactive)
  (insert-translated-name-active "camel"))

(defun insert-translated-name-replace ()
  (interactive)
  (insert-translated-name-replace-symbol
   (cond ((insert-translated-name-match-modes insert-translated-name-line-style-mode-list)
          "line")
         ((insert-translated-name-match-modes insert-translated-name-camel-style-mode-list)
          "camel")
         ((insert-translated-name-match-modes insert-translated-name-underline-style-mode-list)
          "underline")
         (t
          insert-translated-name-default-style))))

(defun insert-translated-name-replace-with-line ()
  (interactive)
  (insert-translated-name-replace-symbol "line"))

(defun insert-translated-name-replace-with-underline ()
  (interactive)
  (insert-translated-name-replace-symbol "underline"))

(defun insert-translated-name-replace-with-camel ()
  (interactive)
  (insert-translated-name-replace-symbol "camel"))

;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;
(defun insert-translated-name-replace-symbol (style)
  (let ((word (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (kill-region (beginning-of-thing 'symbol) (end-of-thing 'symbol)))
    (insert-translated-name-query-translation word style)))

(defun insert-translated-name-match-modes (mode-list)
  (cl-remove-if 'null (mapcar #'(lambda (mode) (derived-mode-p mode)) mode-list)))

(defun insert-translated-name-use-original-translation ()
  (set (make-local-variable 'insert-translated-name-original-translation) t))

(defun insert-translated-name-active (style)
  ;; Enable input method if user has load it.
  (activate-input-method default-input-method)

  ;; Add monitor hook.
  (add-hook 'after-change-functions 'insert-translated-name-monitor-after-change nil t)

  ;; Make sure build hash to contain placeholder.
  (unless (boundp 'insert-translated-name-placeholder-hash)
    (set (make-local-variable 'insert-translated-name-placeholder-hash) (make-hash-table :test 'equal)))

  ;; Make sure clean active overlay first.
  (when (and (boundp 'insert-translated-name-active-overlay)
             insert-translated-name-active-overlay)
    (delete-overlay insert-translated-name-active-overlay))

  ;; Reset active local variables
  (set (make-local-variable 'insert-translated-name-active-point) (point))
  (set (make-local-variable 'insert-translated-name-active-style) style)
  (set (make-local-variable 'insert-translated-name-active-overlay) (make-overlay (point) (point)))

  ;; Active new overlay from current point.
  (overlay-put insert-translated-name-active-overlay 'face 'insert-translated-name-font-lock-mark-word)

  ;; Print play hint.
  (unless (minibuffer-window-active-p (get-buffer-window))
    (message "Type Chinese and press SPACE to translate.")))

(defun insert-translated-name-inactive (&optional keep-style)
  (interactive)
  ;; Disable input method if user has load it.
  (deactivate-input-method)

  ;; Delete active overlay.
  (when (and (boundp 'insert-translated-name-active-overlay)
             insert-translated-name-active-overlay)
    (delete-overlay insert-translated-name-active-overlay))

  ;; Clean active local variables.
  (set (make-local-variable 'insert-translated-name-active-point) nil)
  (when (and (boundp 'insert-translated-name-active-overlay)
             insert-translated-name-active-overlay)
    (set (make-local-variable 'insert-translated-name-active-overlay) nil))

  ;; Clean style.
  (unless keep-style
    (set (make-local-variable 'insert-translated-name-active-style) nil)))

(defun insert-translated-name-monitor-after-change (start end len)
  (when (and (boundp 'insert-translated-name-active-point))
    (if insert-translated-name-active-point
        (let ((translate-start insert-translated-name-active-point)
              (translate-end (point)))
          (cond
           ;; Translate current Chinese words after press SPACE.
           ((string-equal (buffer-substring-no-properties start end) " ")
            (let ((word (buffer-substring-no-properties translate-start translate-end)))
              ;; Delete Chinese words.
              (kill-region translate-start translate-end)

              ;; Query translation.
              (insert-translated-name-query-translation word insert-translated-name-active-style)

              ;; Inactive.
              (insert-translated-name-inactive nil)))
           ;; Update active overlay bound if user press any other non-SPACE character.
           (t
            (move-overlay insert-translated-name-active-overlay translate-start translate-end)))))))

(defun insert-translated-name-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (when (equal point (point))
      (beginning-of-line))
    (parse-partial-sexp (point) point)))

(defun insert-translated-name-in-commit-buffer-p ()
  (and (string-equal (buffer-name) "COMMIT_EDITMSG")
       (save-excursion
         (goto-char (point-min))
         (search-forward-regexp "#\\s-Please\\s-enter\\s-the\\s-commit\\s-message\\s-for\\s-your\\s-changes." nil t))))

(defun insert-translated-name-in-string-p (&optional state)
  (or (nth 3 (or state (insert-translated-name-current-parse-state)))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-string-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
      ))

(defun insert-translated-name-in-comment-p (&optional state)
  (or (nth 4 (or state (insert-translated-name-current-parse-state)))
      (eq (get-text-property (point) 'face) 'font-lock-comment-face)))

(defun insert-translated-name-convert-translation (translation style)
  (let ((words (cl-remove-if #'string-empty-p (split-string translation " "))))
    (cond ((string-equal style "line")
           (string-join (mapcar 'downcase words) "-"))
          ((string-equal style "underline")
           (string-join (mapcar 'downcase words) "_"))
          ((string-equal style "camel")
           (concat (downcase (car words)) (string-join (mapcar 'capitalize (cdr words)))))
          ((or
            (string-equal style "comment")
            (string-equal style "origin"))
           translation))))

(defun insert-translated-name-update-translation-in-buffer (word style translation insert-buffer placeholder)
  (let ((result (insert-translated-name-convert-translation translation style)))
    (save-excursion
      (with-current-buffer insert-buffer
        (let ((placeholder-point (gethash placeholder insert-translated-name-placeholder-hash)))
          (if placeholder-point
              (progn
                ;; Insert result at placeholder point .
                (goto-char placeholder-point)
                (insert result)

                ;; Remove placeholder from hash.
                (remhash placeholder insert-translated-name-placeholder-hash))
            (message (format "Something wrong that we can't found placeholder for %s: %s" word translation))))))))

(defun insert-translated-name-generate-uuid ()
  "Generate a 32 character UUID."
  (md5 (number-to-string (float-time))))

(defun insert-translated-name-query-translation (word style)
  (if (string-equal word "")
      (message "Nothing input, cancel translate.")
    (let ((placeholder (insert-translated-name-generate-uuid)))
      ;; Store placeholder in hash.
      ;; bug: `insert-translated-name-placeholder-hash' is not initialized
      ;; thus I add such fix

      (unless (boundp 'insert-translated-name-placeholder-hash)
        (set (make-local-variable 'insert-translated-name-placeholder-hash) (make-hash-table :test 'equal)))

      (puthash placeholder (point) insert-translated-name-placeholder-hash)

      ;; Query translation.
      (insert-translated-name-retrieve-translation word style placeholder)
      )))

(defun insert-translated-name-process-sentinel (process event)
  (when (string= event "finished\n")
    (with-current-buffer (process-buffer process)
      (let* ((output (buffer-string))
             (first-line (substring-no-properties output 0 (or (string-match "\n" output) (length output)))))
        (insert-translated-name-update-translation-in-buffer
         insert-translated-name-word
         insert-translated-name-style
         (pcase insert-translated-name-program
           ("crow" (alist-get 'translation (json-read-from-string output)))
           ("ollama"
            (pcase insert-translated-name-style
              ("origin" (replace-regexp-in-string "\"" "" (string-trim first-line)))
              (_ (replace-regexp-in-string "\"\\|'\\|‘\\|\\.\\|,\\|，\\|。\\|\\?\\|\\!" "" (string-trim first-line)))
              )))
         insert-translated-name-buffer-name
         insert-translated-name-placeholder)
        ))))

(defvar insert-translated-name-word nil)
(defvar insert-translated-name-style nil)
(defvar insert-translated-name-buffer-name nil)
(defvar insert-translated-name-placeholder nil)

(defun insert-translated-name-retrieve-translation (word style placeholder)
  (setq insert-translated-name-word word)
  (setq insert-translated-name-style style)
  (setq insert-translated-name-buffer-name (buffer-name))
  (setq insert-translated-name-placeholder placeholder)
  (when (get-buffer " *insert-translated-name*")
    (kill-buffer " *insert-translated-name*"))
  (let ((process (pcase insert-translated-name-program
                   ("crow"
                    (start-process
                     "insert-translated-name"
                     " *insert-translated-name*"
                     "crow" "-t" "en" "--json" "-e" insert-translated-name-crow-engine word))
                   ("ollama"
                    (start-process
                     "insert-translated-name"
                     " *insert-translated-name*"
                     "python"
                     insert-translated-name-ollama-file
                     insert-translated-name-ollama-model-name
                     (format "'%s'" word)
                     )))))
    (set-process-sentinel process 'insert-translated-name-process-sentinel)))

(provide 'insert-translated-name)

;;; insert-translated-name.el ends here
