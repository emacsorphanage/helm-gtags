;;; helm-gtags.el --- GNU GLOBAL helm interface

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags
;; Version: 0.4
;; Package-Requires: ((helm "1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(defcustom helm-c-gtags-read-only nil
  "Gtags read only mode."
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
        (push (match-string 1) candidates-list)))
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

(defun helm-c-gtags-type-is-not-file-p (type)
  (not (eq type :file)))

(defun helm-c-gtags-input (type)
  (let ((tagname (helm-c-gtags-token-at-point))
        (prompt (assoc-default type helm-c-gtags-prompt-alist))
        (comp-func (assoc-default type helm-c-gtags-comp-func-alist)))
    (if (and tagname (helm-c-gtags-type-is-not-file-p type))
        (setq prompt (format "%s(default \"%s\") " prompt tagname)))
    (let ((completion-ignore-case helm-c-gtags-ignore-case)
          (completing-read-function 'completing-read-default))
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

(defvar helm-c-gtags-local-directory nil)

(defun helm-c-gtags-base-directory ()
  (or helm-c-gtags-local-directory
      (case helm-c-gtags-path-style
        (root helm-c-global-tag-location)
        (otherwise default-directory))))

(defvar helm-c-gtags-context-stack nil)
(defvar helm-c-gtags-saved-context nil)

(defun helm-c-gtags-save-current-context ()
  (let ((file (buffer-file-name (current-buffer)))
        (curpoint (point)))
    (setf helm-c-gtags-saved-context
          `((:file . ,file)
            (:point . ,curpoint)
            (:readonly . ,buffer-file-read-only)))))

(defvar helm-c-gtags-use-otherwin nil)

(defun helm-c-gtags-open-file (file readonly)
  (if readonly
      (find-file-read-only file)
    (find-file file)))

(defun helm-c-gtags-open-file-other-window (file readonly)
  (setq helm-c-gtags-use-otherwin nil)
  (if readonly
      (find-file-read-only-other-window file)
    (find-file-other-window file)))

(defun helm-c-gtags-pop-context ()
  (let ((context (pop helm-c-gtags-context-stack)))
    (unless context
      (error "helm-gtags: Context stack is empty"))
    (let ((file (assoc-default :file context))
          (curpoint (assoc-default :point context))
          (readonly (assoc-default :readonly context)))
      (helm-c-gtags-open-file file readonly)
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
        (comp-option (or (and comp "-c") ""))
        (local-option (or (and current-prefix-arg
                               (helm-c-gtags-type-is-not-file-p type) "-l") "")))
    (format "%s %s %s %s %s"
            comp-option type-option abs-option case-option local-option)))

(defun helm-c-gtags-construct-command (type &optional in)
  (setq helm-c-gtags-local-directory nil)
  (if (and (helm-c-gtags-type-is-not-file-p type) current-prefix-arg)
      (let ((dir (read-directory-name "Input Directory: ")))
        (setq helm-c-gtags-local-directory (file-name-as-directory dir))))
  (let ((input (or in (helm-c-gtags-input type)))
        (option (helm-c-gtags-construct-option type)))
    (format "global --result=grep %s %s" option input)))

(defun helm-c-gtags-tags-init (&optional input)
  (let ((cmd (helm-c-gtags-construct-command :tag input)))
    (helm-c-gtags-exec-global-command cmd)))

(defun helm-c-gtags-rtags-init (&optional input)
  (let ((cmd (helm-c-gtags-construct-command :rtag input)))
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
         (open-func (if helm-c-gtags-use-otherwin
                        #'helm-c-gtags-open-file-other-window
                      #'helm-c-gtags-open-file))
         (default-directory (helm-c-gtags-base-directory)))
    (funcall open-func filename helm-c-gtags-read-only)
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

(defun helm-gtags-select ()
  (interactive)
  (helm-c-gtags-common '(helm-c-source-gtags-select)))

(defun helm-c-source-gtags-select-tag (candidate)
  `((name . "GNU GLOBAL")
    (init . (lambda ()
              (helm-c-gtags-tags-init ,candidate)))
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-c-gtags-action-openfile)))

(defun helm-c-source-gtags-select-rtag (candidate)
  `((name . "GNU GLOBAL")
    (init . (lambda ()
              (helm-c-gtags-rtags-init ,candidate)))
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-c-gtags-action-openfile)))

(defun helm-c-source-gtags-select-tag-action (c)
  (helm-run-after-quit
   `(lambda ()
      (helm-c-gtags-common (list (helm-c-source-gtags-select-tag ,c))))))

(defun helm-c-source-gtags-select-rtag-action (c)
  (helm-run-after-quit
   `(lambda ()
      (helm-c-gtags-common (list (helm-c-source-gtags-select-rtag ,c))))))

(defvar helm-c-source-gtags-select
  '((name . "GNU GLOBAL SELECT")
    (init .
          (lambda ()
            (with-current-buffer (helm-candidate-buffer 'global)
              (call-process-shell-command "global -c" nil t nil))))
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . (("Goto the location" . helm-c-source-gtags-select-tag-action)
               ("Goto the location(other buffer)" .
                (lambda (c)
                  (setq helm-c-gtags-use-otherwin t)
                  (helm-c-source-gtags-select-tag-action c)))
               ("Move to the referenced point" .
                helm-c-source-gtags-select-rtag-action)))))

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

(defvar helm-c-gtags-mode-name " Helm Gtags")
(defvar helm-c-gtags-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-gtags-mode ()
  "Enable for helm-gtags"
  :group      'helm-gtags
  :init-value nil
  :global     nil
  :keymap     helm-c-gtags-mode-map
  :lighter    helm-c-gtags-mode-name
  (if helm-gtags-mode
      (run-hooks 'helm-gtags-mode-hook)))

(provide 'helm-gtags)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags.el ends here
