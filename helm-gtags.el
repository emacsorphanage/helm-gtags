;;; helm-gtags.el --- GNU GLOBAL helm interface

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags
;; Version: 0.9.2
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
;; `helm-gtags.el' is not compatible `anything-gtags.el', but `helm-gtags.el'
;; is designed for fast search.

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
;;                  (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)
;;                  (local-set-key (kbd "C-c C-f") 'helm-gtags-find-files)))
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'helm)
(require 'helm-files)
(require 'which-func)

(defgroup helm-gtags nil
  "GNU GLOBAL for helm"
  :group 'helm)

(defcustom helm-gtags-path-style 'root
  "Style of file path"
  :type '(choice (const :tag "Root of the current project" root)
                 (const :tag "Relative from the current directory" relative)
                 (const :tag "Absolute Path" absolute))
  :group 'helm-gtags)

(defcustom helm-gtags-ignore-case nil
  "Ignore case in each search."
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-read-only nil
  "Gtags read only mode."
  :type 'boolean
  :group 'helm-gtags)

(defvar helm-gtags-tag-location nil
  "GNU global tag `GTAGS' location")

(defvar helm-gtags-buffer "*helm gtags*")

(defvar helm-gtags-prompt-alist
  '((:tag    . "Find Definition: ")
    (:rtag   . "Find Reference: ")
    (:symbol . "Find Symbol: ")
    (:file   . "Find File: ")))

(defvar helm-gtags-completing-history nil)
(defvar helm-gtags-context-stack (make-hash-table :test 'equal))
(defvar helm-gtags-saved-context nil)
(defvar helm-gtags-use-otherwin nil)
(defvar helm-gtags-local-directory nil)

(defmacro helm-declare-obsolete-variable (old new version)
  `(progn
     (defvaralias ,old ,new)
     (make-obsolete-variable ,old ,new ,version)))

(helm-declare-obsolete-variable
 'helm-c-gtags-path-style 'helm-gtags-path-style "0.8")
(helm-declare-obsolete-variable
 'helm-c-gtags-ignore-case 'helm-gtags-ignore-case  "0.8")
(helm-declare-obsolete-variable
 'helm-c-gtags-read-only 'helm-gtags-read-only "0.8")

;; completsion function for completing-read.
(defun helm-gtags-completing-gtags (string predicate code)
  (helm-gtags-complete :tag string predicate code))
(defun helm-gtags-completing-grtags (string predicate code)
  (helm-gtags-complete :rtag string predicate code))
(defun helm-gtags-completing-gsyms (string predicate code)
  (helm-gtags-complete :symbol string predicate code))
(defun helm-gtags-completing-files (string predicate code)
  (helm-gtags-complete :file string predicate code))

(defvar helm-gtags-comp-func-alist
  '((:tag    . helm-gtags-completing-gtags)
    (:rtag   . helm-gtags-completing-grtags)
    (:symbol . helm-gtags-completing-gsyms)
    (:file   . helm-gtags-completing-files)))

(defun helm-gtags-construct-completion-command (type input)
  (let ((option (helm-gtags-construct-option type t)))
    (format "global %s %s" option input)))

(defun helm-gtags-complete (type string predicate code)
  (let ((candidates-list nil)
        (cmd (helm-gtags-construct-completion-command type string)))
    (with-temp-buffer
      (call-process-shell-command cmd nil t nil)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (push (match-string 1) candidates-list)))
    (if (not code)
        (try-completion string candidates-list predicate)
      (all-completions string candidates-list predicate))))

(defun helm-gtags-token-at-point ()
  (save-excursion
    (let (start)
      (when (looking-at "[a-zA-Z0-9_]")
        (skip-chars-backward "a-zA-Z0-9_")
        (setq start (point))
        (skip-chars-forward "a-zA-Z0-9_")
        (buffer-substring-no-properties start (point))))))

(defsubst helm-gtags-type-is-not-file-p (type)
  (not (eq type :file)))

(defun helm-gtags-input (type)
  (let ((tagname (helm-gtags-token-at-point))
        (prompt (assoc-default type helm-gtags-prompt-alist))
        (comp-func (assoc-default type helm-gtags-comp-func-alist)))
    (when (and tagname (helm-gtags-type-is-not-file-p type))
      (setq prompt (format "%s(default \"%s\") " prompt tagname)))
    (let ((completion-ignore-case helm-gtags-ignore-case)
          (completing-read-function 'completing-read-default))
      (completing-read prompt comp-func nil nil nil
                       'helm-gtags-completing-history tagname))))

(defun helm-gtags-find-tag-directory ()
  (with-temp-buffer
    (let ((status (call-process-shell-command "global -p" nil t)))
      (unless (zerop status)
        (error "GTAGS not found"))
      (goto-char (point-min))
      (let ((tagroot (buffer-substring-no-properties
                      (point) (line-end-position))))
        (setq helm-gtags-tag-location (file-name-as-directory tagroot))))))

(defun helm-gtags-base-directory ()
  (or helm-gtags-local-directory
      (case helm-gtags-path-style
        (root helm-gtags-tag-location)
        (otherwise default-directory))))

(defun helm-gtags-save-current-context ()
  (let ((file (buffer-file-name (current-buffer))))
    (setq helm-gtags-saved-context
          (list :file file :position (point) :readonly buffer-file-read-only))))

(defun helm-gtags-open-file (file readonly)
  (if readonly
      (find-file-read-only file)
    (find-file file)))

(defun helm-gtags-open-file-other-window (file readonly)
  (setq helm-gtags-use-otherwin nil)
  (if readonly
      (find-file-read-only-other-window file)
    (find-file-other-window file)))

(defun helm-gtags-get-context-stack ()
  (let ((tag-location (helm-gtags-find-tag-directory)))
    (gethash tag-location helm-gtags-context-stack)))

(defun helm-gtags-pop-context ()
  (let ((context-stack (helm-gtags-get-context-stack)))
    (unless context-stack
      (error "Context stack is empty(TAG at %s)" helm-gtags-tag-location))
    (let* ((context (pop context-stack))
           (file (plist-get context :file))
           (curpoint (plist-get context :position))
           (readonly (plist-get context :readonly)))
      (puthash helm-gtags-tag-location context-stack helm-gtags-context-stack)
      (helm-gtags-open-file file readonly)
      (goto-char curpoint))))

(defun helm-gtags-exec-global-command (cmd)
  (helm-gtags-find-tag-directory)
  (helm-gtags-save-current-context)
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((default-directory (helm-gtags-base-directory))
          (input (car (last (split-string cmd)))))
      (call-process-shell-command cmd nil t)
      (when (helm-empty-buffer-p (current-buffer))
        (error (format "%s: not found" input))))))

(defvar helm-gtags-command-option-alist
  '((:tag    . "")
    (:rtag   . "-r")
    (:symbol . "-s")
    (:file   . "-Poa")))

(defun helm-gtags-construct-option (type &optional comp)
  (let ((type-opt (assoc-default type helm-gtags-command-option-alist))
        (result-opt (or (and (eq type :file) "") "--result=grep"))
        (abs-opt (or (and (eq helm-gtags-path-style 'absolute) "-a") ""))
        (case-opt (or (and helm-gtags-ignore-case "-i") ""))
        (comp-opt (or (and comp "-c") ""))
        (local-opt (or (and current-prefix-arg
                            (helm-gtags-type-is-not-file-p type) "-l") "")))
    (format "%s %s %s %s %s %s"
            result-opt comp-opt type-opt abs-opt case-opt local-opt)))

(defun helm-gtags-construct-command (type &optional in)
  (setq helm-gtags-local-directory nil)
  (let ((dir (helm-attr 'helm-gtags-base-directory (helm-get-current-source))))
    (when (and dir (helm-gtags-type-is-not-file-p type))
      (setq helm-gtags-local-directory dir)))
  (let ((input (or in (helm-gtags-input type)))
        (option (helm-gtags-construct-option type)))
    (when (string= input "")
      (error "Input is empty!!"))
    (format "global %s %s" option input)))

(defun helm-gtags-tags-init (&optional input)
  (let ((cmd (helm-gtags-construct-command :tag input)))
    (helm-gtags-exec-global-command cmd)))

(defun helm-gtags-rtags-init (&optional input)
  (let ((cmd (helm-gtags-construct-command :rtag input)))
    (helm-gtags-exec-global-command cmd)))

(defun helm-gtags-gsyms-init ()
  (let ((cmd (helm-gtags-construct-command :symbol)))
    (helm-gtags-exec-global-command cmd)))

(defun helm-gtags-files-init ()
  (let ((cmd (helm-gtags-construct-command :file)))
    (helm-gtags-exec-global-command cmd)))

(defun helm-gtags-push-context (context)
  (let ((stack (gethash helm-gtags-tag-location helm-gtags-context-stack)))
    (push context stack)
    (puthash helm-gtags-tag-location stack helm-gtags-context-stack)))

(defun helm-gtags-action-openfile (elm)
  (let* ((elems (split-string elm ":"))
         (filename (first elems))
         (line (string-to-number (second elems)))
         (open-func (if helm-gtags-use-otherwin
                        #'helm-gtags-open-file-other-window
                      #'helm-gtags-open-file))
         (default-directory (helm-gtags-base-directory)))
    (funcall open-func filename helm-gtags-read-only)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-gtags-push-context helm-gtags-saved-context)))

(defun helm-gtags-file-content-at-pos (file pos)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (let ((curfunc (which-function))
            (line (line-number-at-pos))
            (content (or (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))
                         "")))
        (format "%s:%d%s:%s\n"
                file line
                (helm-aif curfunc (format "[%s]" it) "")
                content)))))

(defun helm-gtags-show-stack-init ()
  (let ((context-stack (helm-gtags-get-context-stack)))
    (with-current-buffer (helm-candidate-buffer 'global)
      (loop for context in (reverse context-stack)
            for file = (plist-get context :file)
            for pos  = (plist-get context :position)
            do
            (insert (helm-gtags-file-content-at-pos file pos))))))

(defvar helm-source-gtags-tags
  '((name . "GNU GLOBAL")
    (init . helm-gtags-tags-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-rtags
  '((name . "GNU GLOBAL")
    (init . helm-gtags-rtags-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-gsyms
  '((name . "GNU GLOBAL")
    (init . helm-gtags-gsyms-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-gtags-action-openfile)))

(defun helm-gtags-files-candidate-transformer (file)
  (let ((removed-regexp (format "^%s" helm-gtags-tag-location)))
    (replace-regexp-in-string removed-regexp "" file)))

(defvar helm-source-gtags-files
  '((name . "GNU GLOBAL")
    (init . helm-gtags-files-init)
    (candidates-in-buffer)
    (real-to-display . helm-gtags-files-candidate-transformer)
    (candidate-number-limit . 9999)
    (type . file)))

(defvar helm-source-gtags-show-stack
  '((name . "Show Context Stack")
    (init . helm-gtags-show-stack-init)
    (candidates-in-buffer)
    (real-to-display . helm-gtags-files-candidate-transformer)
    (candidate-number-limit . 9999)
    (action . helm-gtags-action-openfile)))

;;;###autoload
(defun helm-gtags-select ()
  (interactive)
  (helm-gtags-common '(helm-source-gtags-select)))

(defun helm-source-gtags-select-tag (candidate)
  `((name . "GNU GLOBAL")
    (init . (lambda ()
              (helm-gtags-tags-init ,candidate)))
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-gtags-action-openfile)))

(defun helm-source-gtags-select-rtag (candidate)
  `((name . "GNU GLOBAL")
    (init . (lambda ()
              (helm-gtags-rtags-init ,candidate)))
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-gtags-action-openfile)))

(defun helm-source-gtags-select-tag-action (c)
  (helm-run-after-quit
   `(lambda ()
      (helm-gtags-common (list (helm-source-gtags-select-tag ,c))))))

(defun helm-source-gtags-select-rtag-action (c)
  (helm-run-after-quit
   `(lambda ()
      (helm-gtags-common (list (helm-source-gtags-select-rtag ,c))))))

(defvar helm-source-gtags-select
  '((name . "GNU GLOBAL SELECT")
    (init .
          (lambda ()
            (with-current-buffer (helm-candidate-buffer 'global)
              (call-process-shell-command "global -c" nil t nil))))
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . (("Goto the location" . helm-source-gtags-select-tag-action)
               ("Goto the location(other buffer)" .
                (lambda (c)
                  (setq helm-gtags-use-otherwin t)
                  (helm-source-gtags-select-tag-action c)))
               ("Move to the referenced point" .
                helm-source-gtags-select-rtag-action)))))

(defun helm-gtags-searched-directory ()
  (case (prefix-numeric-value current-prefix-arg)
    (4 (let ((dir (read-directory-name "Input Directory: ")))
         (setq helm-gtags-local-directory (file-name-as-directory dir))))
    (16 (file-name-directory (buffer-file-name)))))

(defun helm-gtags-common (srcs)
  (let ((helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t)
        (buf (get-buffer-create helm-gtags-buffer))
        (dir (helm-gtags-searched-directory))
        (src (car srcs)))
    (helm-attrset 'helm-gtags-base-directory dir (symbol-value src))
    (helm-attrset 'name
                  (format "Searched at %s" (or dir default-directory))
                  (symbol-value src))
    (helm :sources srcs :buffer buf)))

;;;###autoload
(defun helm-gtags-find-tag ()
  "Jump to definition"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-tags)))

;;;###autoload
(defun helm-gtags-find-rtag ()
  "Jump to referenced point"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-rtags)))

;;;###autoload
(defun helm-gtags-find-symbol ()
  "Jump to the symbol location"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-gsyms)))

;;;###autoload
(defun helm-gtags-find-files ()
  "Find file with gnu global"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-files)))

;;;###autoload
(defun helm-gtags-pop-stack ()
  "Jump to previous point on the stack"
  (interactive)
  (helm-gtags-pop-context))

;;;###autoload
(defun helm-gtags-show-stack ()
  "Show context stack"
  (interactive)
  (helm-other-buffer 'helm-source-gtags-show-stack
                     (get-buffer-create helm-gtags-buffer)))

;;;###autoload
(defun helm-gtags-clear-stack ()
  "Clear jumped point stack"
  (interactive)
  (let ((tag-location (helm-gtags-find-tag-directory)))
    (puthash tag-location nil helm-gtags-context-stack)))

;;;###autoload
(defun helm-gtags-clear-all-stacks ()
  "Clear all jumped point stacks"
  (interactive)
  (setq helm-gtags-context-stack (make-hash-table :test 'equal)))

(defvar helm-gtags-mode-name " Helm Gtags")
(defvar helm-gtags-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-gtags-mode ()
  "Enable for helm-gtags"
  :group      'helm-gtags
  :init-value nil
  :global     nil
  :keymap     helm-gtags-mode-map
  :lighter    helm-gtags-mode-name
  (when helm-gtags-mode
    (run-hooks 'helm-gtags-mode-hook)))

(provide 'helm-gtags)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags.el ends here
