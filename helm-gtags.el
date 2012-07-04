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

;; `helm-gtags.el' is GNU GLOBAL `helm' interface.
;; `helm-gtags.el' is not compatible `anything-gtags.el'

;;
;; To use this package, add these lines to your init.el or .emacs file:
;;     (require 'helm-config)
;;     (require 'helm-gtags)
;;
;;     (setq helm-c-gtags-path-style 'relative)
;;
;;     (setq gtags-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
;;              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
;;              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
;;              (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))
;;

;;; History:
;; Revision 0.1  2012/07/???? syohex
;; Initial version
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)
(require 'gtags)

(defgroup helm-gtags nil
  "GNU GLOBAL for helm"
  :group 'helm)

(defcustom helm-c-gtags-path-style 'root
  "Style of file path"
  :type '(choice (const :tag "Root of the current project" root)
                 (const :tag "Relative from the current directory" relative)
                 (const :tag "Absolute Path" absolute))
  :group 'helm-gtags)


(defcustom helm-c-gtags-ignore-case nil
  "Ignore case in each search."
  :type 'boolean
  :group 'helm-gtags)

(defvar helm-c-global-tag-location nil
  "GNU global tag `GTAGS' location")

(defvar helm-c-gtags-buffer "*helm gtags*")

(defvar helm-c-gtags-prompt-alist
  '((:tag    . "Find Definition: ")
    (:rtag   . "Find Reference: ")
    (:symbol . "Find Symbol: ")
    (:file   . "Find File:")))

(defvar helm-c-gtags-comp-func-alist
  '((:tag    . gtags-completing-gtags)
    (:rtag   . gtags-completing-grtags)
    (:symbol . gtags-completing-gsyms)
    (:file   . gtags-completing-files)))

(defun helm-c-source-gtags-input (type)
  (let ((tagname (gtags-current-token))
        (prompt (assoc-default type helm-c-gtags-prompt-alist))
        (comp-func (assoc-default type helm-c-gtags-comp-func-alist)))
    (if tagname
        (setq prompt (format "%s(default \"%s\") " prompt tagname)))
    (message prompt)
    (completing-read prompt comp-func nil nil nil gtags-history-list tagname)))

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

(defun helm-c-source-base-directory ()
  (case helm-c-gtags-path-style
    (root helm-c-global-tag-location)
    (otherwise default-directory)))

(defun helm-c-source-exec-global-command (cmd)
  (helm-c-source-gtags-find-tag-directory)
  (gtags-push-context)
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((default-directory (helm-c-source-base-directory)))
      (call-process-shell-command cmd nil t nil))))

(defvar helm-c-gtags-command-option-alist
  '((:tag    . "")
    (:rtag   . "-r")
    (:symbol . "-s")
    (:file   . "-Po")))

(defun helm-c-source-gtags-construct-option (type)
  (let ((type-option (assoc-default type helm-c-gtags-command-option-alist))
        (abs-option (or (and (eq helm-c-gtags-path-style 'absolute) "-a") ""))
        (case-option (or (and helm-c-gtags-ignore-case "-i") "")))
    (format "%s %s %s" type-option abs-option case-option)))

(defun helm-c-source-gtags-construct-command (type)
  (let ((input (helm-c-source-gtags-input type))
        (option (helm-c-source-gtags-construct-option type)))
    (format "global --result=grep %s %s" option input)))

(defun helm-c-source-gtags-tags-init ()
  (let ((cmd (helm-c-source-gtags-construct-command :tag)))
    (helm-c-source-exec-global-command cmd)))

(defun helm-c-source-gtags-rtags-init ()
  (let ((cmd (helm-c-source-gtags-construct-command :rtag)))
    (helm-c-source-exec-global-command cmd)))

(defun helm-c-source-gtags-gsyms-init ()
  (let ((cmd (helm-c-source-gtags-construct-command :symbol)))
    (helm-c-source-exec-global-command cmd)))

(defun helm-c-source-gtags-files-init ()
  (let ((cmd (helm-c-source-gtags-construct-command :file)))
    (helm-c-source-exec-global-command cmd)))

(defun helm-c-gtags-action-openfile (elm)
  (let* ((elems (split-string elm ":"))
         (filename (first elems))
         (line (string-to-number (second elems)))
         (default-directory (helm-c-source-base-directory)))
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
