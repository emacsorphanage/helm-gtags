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
;;     (add-hook 'helm-gtags-mode-hook
;;               '(lambda ()
;;                  (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
;;                  (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
;;                  (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
;;                  (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))
;;

;;; History:
;; Revision 0.1  2012/07/???? syohex
;; Initial version
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)

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

;; completsion function for completing-read.
(defun helm-c-gtags-completing-gtags (string predicate code)
  (helm-c-gtags-complete :tag string predicate code))
(defun helm-c-gtags-completing-grtags (string predicate code)
  (helm-c-gtags-complete :rtag string predicate code))
(defun helm-c-gtags-completing-gsyms (string predicate code)
  (helm-c-gtags-complete :symbol string predicate code))
(defun helm-c-gtags-completing-files (string predicate code)
  (helm-c-gtags-complete :file string predicate code))

(defvar helm-c-gtags-comp-func-alist
  '((:tag    . helm-c-gtags-completing-gtags)
    (:rtag   . helm-c-gtags-completing-grtags)
    (:symbol . helm-c-gtags-completing-gsyms)
    (:file   . helm-c-gtags-completing-files)))

(defun helm-c-gtags-construct-completion-command (type input)
  (let ((option (helm-c-gtags-construct-option type t)))
    (format "global %s %s" option input)))

(defun helm-c-gtags-complete (type string predicate code)
  (let ((candidates-list nil)
        (cmd (helm-c-gtags-construct-completion-command type string)))
    (with-temp-buffer
      (call-process-shell-command cmd nil t nil)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (push (buffer-substring-no-properties
               (match-beginning 1) (match-end 1)) candidates-list)))
    (cond ((eq code nil)
           (try-completion string candidates-list predicate))
          ((eq code t)
           (all-completions string candidates-list predicate)))))

(defun helm-c-gtags-token-at-point ()
  (save-excursion
    (let (start)
      (if (looking-at "[a-zA-Z0-9_]")
          (progn
            (skip-chars-backward "a-zA-Z0-9_")
            (setq start (point))
            (skip-chars-forward "a-zA-Z0-9_")
            (buffer-substring-no-properties start (point)))))))

(defun helm-c-gtags-input (type)
  (let ((tagname (helm-c-gtags-token-at-point))
        (prompt (assoc-default type helm-c-gtags-prompt-alist))
        (comp-func (assoc-default type helm-c-gtags-comp-func-alist)))
    (if tagname
        (setq prompt (format "%s(default \"%s\") " prompt tagname)))
    (message prompt)
    (let ((completion-ignore-case helm-c-gtags-ignore-case))
      (completing-read prompt comp-func nil nil nil nil tagname))))

(defun helm-c-gtags-find-tag-directory ()
  (with-temp-buffer
    (let ((status (call-process "global" nil t nil "-p")))
      (unless (= status 0)
        (error "GTAGS not found"))
      (goto-char (point-min))
      (let ((cur (point)))
        (end-of-line)
        (setq helm-c-global-tag-location
              (file-name-as-directory (buffer-substring cur (point))))))))

(defun helm-c-gtags-base-directory ()
  (case helm-c-gtags-path-style
    (root helm-c-global-tag-location)
    (otherwise default-directory)))

(defvar helm-c-gtags-context-stack nil)
(defvar helm-c-gtags-saved-context nil)

(defun helm-c-gtags-save-current-context ()
  (let ((file (buffer-file-name (current-buffer)))
        (curpoint (point)))
    (setf helm-c-gtags-saved-context (cons file curpoint))))

(defun helm-c-gtags-pop-context ()
  (let ((file-and-point (pop helm-c-gtags-context-stack)))
    (unless file-and-point
      (error "helm-gtags: Context stack is empty"))
    (let ((file (car file-and-point))
          (curpoint (cdr file-and-point)))
      (find-file file)
      (goto-char curpoint))))

(defun helm-c-gtags-exec-global-command (cmd)
  (helm-c-gtags-find-tag-directory)
  (helm-c-gtags-save-current-context)
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((default-directory (helm-c-gtags-base-directory))
          (input (car (last (split-string cmd)))))
      (call-process-shell-command cmd nil t nil)
      (if (helm-empty-buffer-p (current-buffer))
          (error (format "%s: not found" input))))))

(defvar helm-c-gtags-command-option-alist
  '((:tag    . "")
    (:rtag   . "-r")
    (:symbol . "-s")
    (:file   . "-Po")))

(defun helm-c-gtags-construct-option (type &optional comp)
  (let ((type-option (assoc-default type helm-c-gtags-command-option-alist))
        (abs-option (or (and (eq helm-c-gtags-path-style 'absolute) "-a") ""))
        (case-option (or (and helm-c-gtags-ignore-case "-i") ""))
        (comp-option (or (and comp "-c") "")))
    (format "%s %s %s %s" comp-option type-option abs-option case-option)))

(defun helm-c-gtags-construct-command (type)
  (let ((input (helm-c-gtags-input type))
        (option (helm-c-gtags-construct-option type)))
    (format "global --result=grep %s %s" option input)))

(defun helm-c-gtags-tags-init ()
  (let ((cmd (helm-c-gtags-construct-command :tag)))
    (helm-c-gtags-exec-global-command cmd)))

(defun helm-c-gtags-rtags-init ()
  (let ((cmd (helm-c-gtags-construct-command :rtag)))
    (helm-c-gtags-exec-global-command cmd)))

(defun helm-c-gtags-gsyms-init ()
  (let ((cmd (helm-c-gtags-construct-command :symbol)))
    (helm-c-gtags-exec-global-command cmd)))

(defun helm-c-gtags-files-init ()
  (let ((cmd (helm-c-gtags-construct-command :file)))
    (helm-c-gtags-exec-global-command cmd)))

(defun helm-c-gtags-action-openfile (elm)
  (let* ((elems (split-string elm ":"))
         (filename (first elems))
         (line (string-to-number (second elems)))
         (default-directory (helm-c-gtags-base-directory)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (push helm-c-gtags-saved-context helm-c-gtags-context-stack)))

(defvar helm-c-source-gtags-tags
  '((name . "GNU GLOBAL")
    (init . helm-c-gtags-tags-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-c-gtags-action-openfile)))

(defvar helm-c-source-gtags-rtags
  '((name . "GNU GLOBAL")
    (init . helm-c-gtags-rtags-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-c-gtags-action-openfile)))

(defvar helm-c-source-gtags-gsyms
  '((name . "GNU GLOBAL")
    (init . helm-c-gtags-gsyms-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-c-gtags-action-openfile)))

(defvar helm-c-source-gtags-files
  '((name . "GNU GLOBAL")
    (init . helm-c-gtags-files-init)
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
  (helm-c-gtags-pop-context))

(defvar helm-c-gtags-mode-name "Helm-Gtags")
(defvar helm-c-gtags-mode-map (make-sparse-keymap))

(defun helm-gtags-mode ()
  "Major mode for helm-gtags"
  (interactive)
  (setq mode-name helm-c-gtags-mode-name)
  (setq major-mode 'helm-gtags-mode)
  (use-local-map helm-c-gtags-mode-map)
  (run-hooks 'helm-gtags-mode-hook))

(provide 'helm-gtags)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags.el ends here
