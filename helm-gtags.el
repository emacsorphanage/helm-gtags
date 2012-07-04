;;; helm-gtags.el --- GNU GLOBAL helm interface

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; History:
;; Revision 0.1  2012/07/???? syohex
;; Initial version
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)
(require 'gtags)

(defvar helm-c-global-tag-location nil
  "GNU global tag `GTAGS' location")

(defvar helm-c-gtags-buffer "*helm gtags*")

(defun helm-c-source-gtags-tags-completion (prompt)
  (completing-read prompt 'gtags-completing-gtags
                   nil nil nil gtags-history-list))

(defun helm-c-source-gtags-rtags-completion (prompt)
  (completing-read prompt 'gtags-completing-grtags
                   nil nil nil gtags-history-list))

(defun helm-c-source-gtags-gsyms-completion (prompt)
  (completing-read prompt 'gtags-completing-gsyms
                   nil nil nil gtags-history-list))

(defun helm-c-source-gtags-files-completion (prompt)
  (completing-read prompt 'gtags-completing-files
                   nil nil nil gtags-history-list))

(defun helm-c-source-gtags-find-tag-directory ()
  (with-temp-buffer
    (let ((status (call-process "global" nil t nil "-p")))
      (unless (= status 0)
        (error "GTAGS not found"))
      (goto-char (point-min))
      (let ((cur (point)))
        (end-of-line)
        (setq helm-c-global-tag-location
              (file-name-as-directory (buffer-substring cur (point))))))))

(defun helm-c-source-exec-global-command (cmd)
  (helm-c-source-gtags-find-tag-directory)
  (gtags-push-context)
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((default-directory helm-c-global-tag-location))
      (call-process-shell-command cmd nil t nil))))

(defun helm-c-source-gtags-tags-init ()
  (let ((input (helm-c-source-gtags-tags-completion "Find Definition: ")))
    (helm-c-source-exec-global-command
     (format "global --result=grep %s" input))))

(defun helm-c-source-gtags-rtags-init ()
  (let ((input (helm-c-source-gtags-rtags-completion "Find Reference: ")))
    (helm-c-source-exec-global-command
     (format "global --result=grep -r %s" input))))

(defun helm-c-source-gtags-gsyms-init ()
  (let ((input (helm-c-source-gtags-gsyms-completion "Find Symbol: ")))
    (helm-c-source-exec-global-command
     (format "global --result=grep -s %s" input))))

(defun helm-c-source-gtags-files-init ()
  (let ((input (helm-c-source-gtags-files-completion "Find File: ")))
    (helm-c-source-exec-global-command
     (format "global --result=grep -Po %s" input))))

(defun helm-c-gtags-action-openfile (elm)
  (let* ((elems (split-string elm ":"))
         (filename (first elems))
         (line (string-to-number (second elems)))
         (default-directory helm-c-global-tag-location))
    (find-file filename)
    (forward-line (1- line))))

(defvar helm-c-source-gtags-tags
  '((name . "GNU GLOBAL")
    (init . helm-c-source-gtags-tags-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-c-gtags-action-openfile)))

(defvar helm-c-source-gtags-rtags
  '((name . "GNU GLOBAL")
    (init . helm-c-source-gtags-rtags-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-c-gtags-action-openfile)))

(defvar helm-c-source-gtags-gsyms
  '((name . "GNU GLOBAL")
    (init . helm-c-source-gtags-gsyms-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-c-gtags-action-openfile)))

(defvar helm-c-source-gtags-files
  '((name . "GNU GLOBAL")
    (init . helm-c-source-gtags-files-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-c-gtags-action-openfile)))

(defun helm-c-gtags-common (srcs)
  (let ((helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t)
        (buf (get-buffer-create helm-c-gtags-buffer)))
    (helm :sources srcs
          :buffer buf)))

(defun helm-gtags-find-tag ()
  (interactive)
  (helm-c-gtags-common '(helm-c-source-gtags-tags)))

(defun helm-gtags-find-rtag ()
  (interactive)
  (helm-c-gtags-common '(helm-c-source-gtags-rtags)))

(defun helm-gtags-find-symbol ()
  (interactive)
  (helm-c-gtags-common '(helm-c-source-gtags-gsyms)))

(defun helm-gtags-find-files ()
  (interactive)
  (helm-c-gtags-common '(helm-c-source-gtags-files)))

(defun helm-gtags-pop-stack ()
  (interactive)
  (gtags-pop-stack))

(provide 'helm-gtags)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags.el ends here
