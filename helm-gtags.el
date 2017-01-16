;;; helm-gtags.el --- GNU GLOBAL helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags
;; Version: 1.5.6
;; Package-Requires: ((emacs "24.4") (helm "2.0"))

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

;; `helm-gtags.el' is a `helm' interface of GNU Global.
;; `helm-gtags.el' is not compatible `anything-gtags.el', but `helm-gtags.el'
;; is designed for fast search.

;;
;; To use this package, add these lines to your init.el or .emacs file:
;;
;;     ;; Enable helm-gtags-mode
;;     (add-hook 'c-mode-hook 'helm-gtags-mode)
;;     (add-hook 'c++-mode-hook 'helm-gtags-mode)
;;     (add-hook 'asm-mode-hook 'helm-gtags-mode)
;;
;;     ;; Set key bindings
;;     (eval-after-load "helm-gtags"
;;       '(progn
;;          (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;;          (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;;          (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;;          (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;;          (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;          (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;          (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))
;;

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-files)
(require 'which-func)
(require 'pulse)
(require 'subr-x)

(declare-function helm-comp-read "helm-mode")
(declare-function cygwin-convert-file-name-from-windows "cygw32.c")
(declare-function cygwin-convert-file-name-to-windows "cygw32.c")

(defgroup helm-gtags nil
  "GNU GLOBAL for helm."
  :group 'helm)

(defcustom helm-gtags-path-style 'root
  "Style of file path"
  :type '(choice (const :tag "Root of the current project" root)
                 (const :tag "Relative from the current directory" relative)
                 (const :tag "Absolute Path" absolute)))

(defcustom helm-gtags-ignore-case nil
  "Ignore case in each search."
  :type 'boolean)

(defcustom helm-gtags-cygwin-use-global-w32-port t
  "Use the GNU global win32 port in Cygwin."
  :type 'boolean)

(defcustom helm-gtags-read-only nil
  "Gtags read only mode."
  :type 'boolean)

(defcustom helm-gtags-auto-update nil
  "*If non-nil, tag files are updated whenever a file is saved."
  :type 'boolean)

(defcustom helm-gtags-pulse-at-cursor t
  "If non-nil, pulse at point after jumping"
  :type 'boolean)

(defcustom helm-gtags-cache-select-result nil
  "*If non-nil, results of helm-gtags-select and helm-gtags-select-path are cached."
  :type 'boolean)

(defcustom helm-gtags-cache-max-result-size (* 10 1024 1024) ;10M
  "Max size(bytes) to cache for each select result."
  :type 'integer)

(defcustom helm-gtags-update-interval-second 60
  "Tags are updated in `after-save-hook' if this seconds is passed from last update.
Always update if value of this variable is nil."
  :type '(choice (integer :tag "Update interval seconds")
                 (boolean :tag "Update every time" nil)))

(defcustom helm-gtags-highlight-candidate t
  "Highlight candidate or not"
  :type 'boolean)

(defcustom helm-gtags-use-input-at-cursor nil
  "Use input at cursor"
  :type 'boolean)

(defcustom helm-gtags-prefix-key "\C-c"
  "If non-nil, it is used for the prefix key of gtags-xxx command."
  :type 'string)

(defcustom helm-gtags-suggested-key-mapping nil
  "If non-nil, suggested key mapping is enabled."
  :type 'boolean)

(defcustom helm-gtags-preselect nil
  "If non-nil, preselect current file and line."
  :type 'boolean)

(defcustom helm-gtags-display-style nil
  "Style of display result."
  :type '(choice (const :tag "Show in detail" detail)
                 (const :tag "Normal style" nil)))

(defcustom helm-gtags-fuzzy-match nil
  "Enable fuzzy match"
  :type 'boolean)

(defcustom helm-gtags-maximum-candidates (if helm-gtags-fuzzy-match 100 9999)
  "Maximum number of helm candidates"
  :type 'integer)

(defcustom helm-gtags-direct-helm-completing nil
  "Use helm mode directly."
  :type 'boolean)

(defface helm-gtags-file
  '((t :inherit font-lock-keyword-face))
  "Face for line numbers in the error list.")

(defface helm-gtags-lineno
  '((t :inherit font-lock-doc-face))
  "Face for line numbers in the error list.")

(defface helm-gtags-match
  '((t :inherit helm-match))
  "Face for word matched against tagname")

(defvar helm-gtags--tag-location nil)
(defvar helm-gtags--last-update-time 0)
(defvar helm-gtags--completing-history nil)
(defvar helm-gtags--context-stack (make-hash-table :test 'equal))
(defvar helm-gtags--result-cache (make-hash-table :test 'equal))
(defvar helm-gtags--saved-context nil)
(defvar helm-gtags--use-otherwin nil)
(defvar helm-gtags--local-directory nil)
(defvar helm-gtags--parsed-file nil)
(defvar helm-gtags--current-position nil)
(defvar helm-gtags--real-tag-location nil)
(defvar helm-gtags--last-input nil)
(defvar helm-gtags--query nil)
(defvar helm-gtags--last-default-directory nil)

(defconst helm-gtags--buffer "*helm gtags*")

(defconst helm-gtags--include-regexp
  "\\`\\s-*#\\(?:include\\|import\\)\\s-*[\"<]\\(?:[./]*\\)?\\(.*?\\)[\">]")

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
(defun helm-gtags--completing-gtags (string predicate code)
  (helm-gtags--complete 'tag string predicate code))
(defun helm-gtags--completing-pattern (string predicate code)
  (helm-gtags--complete 'pattern string predicate code))
(defun helm-gtags--completing-grtags (string predicate code)
  (helm-gtags--complete 'rtag string predicate code))
(defun helm-gtags--completing-gsyms (string predicate code)
  (helm-gtags--complete 'symbol string predicate code))
(defun helm-gtags--completing-files (string predicate code)
  (helm-gtags--complete 'find-file string predicate code))

(defconst helm-gtags-comp-func-alist
  '((tag       . helm-gtags--completing-gtags)
    (pattern   . helm-gtags--completing-pattern)
    (rtag      . helm-gtags--completing-grtags)
    (symbol    . helm-gtags--completing-gsyms)
    (find-file . helm-gtags--completing-files)))

(defconst helm-gtags--search-option-alist
  '((pattern   . "-g")
    (rtag      . "-r")
    (symbol    . "-s")
    (find-file . "-Poa")))

(defsubst helm-gtags--windows-p ()
  (memq system-type '(windows-nt ms-dos)))

(defun helm-gtags--remove-carrige-returns ()
  (when (helm-gtags--windows-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\xd" nil t)
        (replace-match "")))))

;; Work around for GNU global Windows issue
(defsubst helm-gtags--use-abs-path-p (gtagslibpath)
  (and (helm-gtags--windows-p) gtagslibpath))

(defun helm-gtags--construct-options (type completion)
  (let ((find-file-p (eq type 'find-file))
        (gtagslibpath (getenv "GTAGSLIBPATH"))
        options)
    (unless find-file-p
      (push "--result=grep" options))
    (when completion
      (push "-c" options))
    (helm-aif (assoc-default type helm-gtags--search-option-alist)
        (push it options))
    (when (or (eq helm-gtags-path-style 'absolute)
              (helm-gtags--use-abs-path-p gtagslibpath))
      (push "-a" options))
    (when helm-gtags-ignore-case
      (push "-i" options))
    (when (and current-prefix-arg (not find-file-p))
      (push "-l" options))
    (when gtagslibpath
      (push "-T" options))
    options))

(defun helm-gtags--complete (type string predicate code)
  (let* ((options (helm-gtags--construct-options type t))
         (args (reverse (cons string options)))
         candidates)
    (with-temp-buffer
      (apply #'process-file "global" nil t nil args)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (push (match-string-no-properties 1) candidates)))
    (if (not code)
        (try-completion string candidates predicate)
      (all-completions string candidates predicate))))

(defun helm-gtags--token-at-point (type)
  (if (not (eq type 'find-file))
      (thing-at-point 'symbol)
    (let ((line (helm-current-line-contents)))
      (when (string-match helm-gtags--include-regexp line)
        (match-string-no-properties 1 line)))))

(defconst helm-gtags--prompt-alist
  '((tag       . "Find Definition: ")
    (pattern   . "Find Pattern: ")
    (rtag      . "Find Reference: ")
    (symbol    . "Find Symbol: ")
    (find-file . "Find File: ")))

(defun helm-gtags--read-tagname (type &optional default-tagname)
  (let ((tagname (helm-gtags--token-at-point type))
        (prompt (assoc-default type helm-gtags--prompt-alist))
        (comp-func (assoc-default type helm-gtags-comp-func-alist)))
    (if (and tagname helm-gtags-use-input-at-cursor)
        tagname
      (when (and (not tagname) default-tagname)
        (setq tagname default-tagname))
      (when tagname
        (setq prompt (format "%s(default \"%s\") " prompt tagname)))
      (let ((completion-ignore-case helm-gtags-ignore-case)
            (completing-read-function 'completing-read-default))
        (if (and helm-gtags-direct-helm-completing (memq type '(tag rtag symbol find-file)))
            (helm-comp-read prompt comp-func
                            :history 'helm-gtags--completing-history
                            :exec-when-only-one t
                            :default tagname)
          (completing-read prompt comp-func nil nil nil
                           'helm-gtags--completing-history tagname))))))

(defun helm-gtags--path-libpath-p (tagroot)
  (helm-aif (getenv "GTAGSLIBPATH")
      (cl-loop for path in (parse-colon-path it)
               for libpath = (file-name-as-directory (expand-file-name path))
               thereis (string= tagroot libpath))))

(defsubst helm-gtags--convert-cygwin-windows-file-name-p ()
  (and (eq system-type 'cygwin) helm-gtags-cygwin-use-global-w32-port))

(defun helm-gtags--tag-directory ()
  (with-temp-buffer
    (helm-aif (getenv "GTAGSROOT")
        it
      (unless (zerop (process-file "global" nil t nil "-p"))
        (error "GTAGS not found"))
      (goto-char (point-min))
      (when (looking-at "^\\([^\r\n]+\\)")
        (let ((tag-path (match-string-no-properties 1)))
          (file-name-as-directory
           (if (helm-gtags--convert-cygwin-windows-file-name-p)
               (cygwin-convert-file-name-from-windows tag-path)
             tag-path)))))))

(defun helm-gtags--find-tag-directory ()
  (setq helm-gtags--real-tag-location nil)
  (let ((tagroot (helm-gtags--tag-directory)))
    (if (and (helm-gtags--path-libpath-p tagroot) helm-gtags--tag-location)
        (progn
          (setq helm-gtags--real-tag-location tagroot)
          helm-gtags--tag-location)
      (setq helm-gtags--tag-location tagroot))))

(defun helm-gtags--base-directory ()
  (let ((dir (or helm-gtags--last-default-directory
                 helm-gtags--local-directory
                 (cl-case helm-gtags-path-style
                   (root (or helm-gtags--real-tag-location
                             helm-gtags--tag-location))
                   (otherwise default-directory))))
        (remote (file-remote-p default-directory)))
    (if (and remote (not (file-remote-p dir)))
        (concat remote dir)
      dir)))

(defsubst helm-gtags--new-context-info (index stack)
  (list :index index :stack stack))

(defun helm-gtags--put-context-stack (tag-location index stack)
  (puthash tag-location (helm-gtags--new-context-info index stack)
           helm-gtags--context-stack))

(defsubst helm-gtags--current-context ()
  (let ((file (buffer-file-name (current-buffer))))
    (list :file file :position (point) :readonly buffer-file-read-only)))

(defsubst helm-gtags--save-current-context ()
  (setq helm-gtags--saved-context (helm-gtags--current-context)))

(defun helm-gtags--open-file (file readonly)
  (if readonly
      (find-file-read-only file)
    (find-file file)))

(defun helm-gtags--open-file-other-window (file readonly)
  (setq helm-gtags--use-otherwin nil)
  (if readonly
      (find-file-read-only-other-window file)
    (find-file-other-window file)))

(defun helm-gtags--get-context-info ()
  (let* ((tag-location (helm-gtags--find-tag-directory))
         (context-info (gethash tag-location helm-gtags--context-stack))
         (context-stack (plist-get context-info :stack)))
    (if (null context-stack)
        (error "Context stack is empty(TAG at %s)" tag-location)
      context-info)))

(defun helm-gtags--get-or-create-context-info ()
  (or (gethash helm-gtags--tag-location helm-gtags--context-stack)
      (helm-gtags--new-context-info -1 nil)))

;;;###autoload
(defun helm-gtags-clear-all-cache ()
  (interactive)
  (clrhash helm-gtags--result-cache))

;;;###autoload
(defun helm-gtags-clear-cache ()
  (interactive)
  (helm-gtags--find-tag-directory)
  (let* ((tag-location (or helm-gtags--real-tag-location
                           helm-gtags--tag-location))
         (gtags-path (concat tag-location "GTAGS"))
         (gpath-path (concat tag-location "GPATH")))
    (remhash gtags-path helm-gtags--result-cache)
    (remhash gpath-path helm-gtags--result-cache)))

(defun helm-gtags--move-to-context (context)
  (let ((file (plist-get context :file))
        (curpoint (plist-get context :position))
        (readonly (plist-get context :readonly)))
    (helm-gtags--open-file file readonly)
    (goto-char curpoint)
    (recenter)))

;;;###autoload
(defun helm-gtags-next-history ()
  "Jump to next position on context stack"
  (interactive)
  (let* ((context-info (helm-gtags--get-context-info))
         (current-index (plist-get context-info :index))
         (context-stack (plist-get context-info :stack))
         context)
    (when (<= current-index -1)
      (error "This context is latest in context stack"))
    (setf (nth current-index context-stack) (helm-gtags--current-context))
    (cl-decf current-index)
    (if (= current-index -1)
        (setq context helm-gtags--current-position
              helm-gtags--current-position nil)
      (setq context (nth current-index context-stack)))
    (helm-gtags--put-context-stack helm-gtags--tag-location
                                   current-index context-stack)
    (helm-gtags--move-to-context context)))

;;;###autoload
(defun helm-gtags-previous-history ()
  "Jump to previous position on context stack"
  (interactive)
  (let* ((context-info (helm-gtags--get-context-info))
         (current-index (plist-get context-info :index))
         (context-stack (plist-get context-info :stack))
         (context-length (length context-stack)))
    (cl-incf current-index)
    (when (>= current-index context-length)
      (error "This context is last in context stack"))
    (if (= current-index 0)
        (setq helm-gtags--current-position (helm-gtags--current-context))
      (setf (nth (- current-index 1) context-stack) (helm-gtags--current-context)))
    (let ((prev-context (nth current-index context-stack)))
      (helm-gtags--move-to-context prev-context))
    (helm-gtags--put-context-stack helm-gtags--tag-location
                                   current-index context-stack)))

(defun helm-gtags--get-result-cache (file)
  (helm-gtags--find-tag-directory)
  (let* ((file-path (concat (or helm-gtags--real-tag-location
                                helm-gtags--tag-location)
                            file))
         (file-mtime (nth 5 (file-attributes file-path)))
         (hash-value (gethash file-path helm-gtags--result-cache))
         (cached-file-mtime (nth 0 hash-value)))
    (if (and cached-file-mtime (equal cached-file-mtime file-mtime))
        (nth 1 hash-value)
      nil)))

(defun helm-gtags--put-result-cache (file cache)
  (helm-gtags--find-tag-directory)
  (let* ((file-path (concat (or helm-gtags--real-tag-location
                                helm-gtags--tag-location)
                            file))
         (file-mtime (nth 5 (file-attributes file-path)))
         (hash-value (list file-mtime cache)))
    (puthash file-path hash-value helm-gtags--result-cache)))

(defun helm-gtags--referer-function (file ref-line)
  (let ((is-opened (cl-loop with path = (concat default-directory file)
                            for buf in (buffer-list)
                            when (string= (buffer-file-name buf) path)
                            return it))
        retval)
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (forward-line (1- ref-line))
      (unless (zerop (current-indentation))
        (setq retval (which-function)))
      (unless is-opened
        (kill-buffer (current-buffer)))
      retval)))

(defun helm-gtags--show-detail ()
  (goto-char (point-min))
  (while (not (eobp))
    (let ((line (helm-current-line-contents)))
      (let* ((file-and-line (helm-gtags--extract-file-and-line line))
             (file (car file-and-line))
             (ref-line (cdr file-and-line))
             (ref-func (helm-gtags--referer-function file ref-line)))
        (when ref-func
          (search-forward ":" nil nil 2)
          (insert " " ref-func "|"))
        (forward-line 1)))))

(defun helm-gtags--print-path-in-gtagslibpath (args)
  (let ((libpath (getenv "GTAGSLIBPATH")))
    (when libpath
      (dolist (path (parse-colon-path libpath))
        (let ((default-directory (file-name-as-directory path)))
          (apply #'process-file "global" nil t nil "-Poa" args))))))

(defun helm-gtags--exec-global-command (type input &optional detail)
  (let ((args (helm-gtags--construct-command type input)))
    (helm-gtags--find-tag-directory)
    (helm-gtags--save-current-context)
    (let ((buf-coding buffer-file-coding-system))
      (with-current-buffer (helm-candidate-buffer 'global)
        (let ((default-directory (helm-gtags--base-directory))
              (input (car (last args)))
              (coding-system-for-read buf-coding)
              (coding-system-for-write buf-coding))
          (unless (zerop (apply #'process-file "global" nil '(t nil) nil args))
            (error (format "%s: not found" input)))
          ;; --path options does not support searching under GTAGSLIBPATH
          (when (eq type 'find-file)
            (helm-gtags--print-path-in-gtagslibpath args))
          (helm-gtags--remove-carrige-returns)
          (when detail
            (helm-gtags--show-detail)))))))

(defun helm-gtags--construct-command (type &optional in)
  (setq helm-gtags--local-directory nil)
  (let ((dir (helm-attr 'helm-gtags-base-directory (helm-get-current-source))))
    (when (and dir (not (eq type 'find-file)))
      (setq helm-gtags--local-directory dir)))
  (let ((input (or in helm-gtags--query))
        (options (helm-gtags--construct-options type nil)))
    (when (string-empty-p input)
      (error "Input is empty!!"))
    (setq helm-gtags--last-input input)
    (reverse (cons input options))))

(defun helm-gtags--tags-init (&optional input)
  (helm-gtags--exec-global-command 'tag input))

(defun helm-gtags--pattern-init (&optional input)
  (helm-gtags--exec-global-command 'pattern input helm-gtags-display-style))

(defun helm-gtags--rtags-init (&optional input)
  (helm-gtags--exec-global-command 'rtag input helm-gtags-display-style))

(defun helm-gtags--gsyms-init ()
  (helm-gtags--exec-global-command 'symbol nil helm-gtags-display-style))

(defun helm-gtags--files-init ()
  (helm-gtags--exec-global-command 'find-file nil))

(defun helm-gtags--real-file-name ()
  (let ((buffile (buffer-file-name)))
    (unless buffile
      (error "This buffer is not related to file."))
    (if (file-remote-p buffile)
        (tramp-file-name-localname (tramp-dissect-file-name buffile))
      (file-truename buffile))))

(defun helm-gtags--find-tag-from-here-init ()
  (helm-gtags--find-tag-directory)
  (helm-gtags--save-current-context)
  (let ((token (helm-gtags--token-at-point 'from-here)))
    (unless token
      (error "Cursor is not on symbol."))
    (let* ((filename (helm-gtags--real-file-name))
           (from-here-opt (format "--from-here=%d:%s"
                                  (line-number-at-pos)
                                  (if (helm-gtags--convert-cygwin-windows-file-name-p)
                                      (cygwin-convert-file-name-to-windows filename)
                                    filename))))
      (setq helm-gtags--last-input token)
      (with-current-buffer (helm-candidate-buffer 'global)
        (let* ((default-directory (helm-gtags--base-directory))
               (status (process-file "global" nil '(t nil) nil
                                     "--result=grep" from-here-opt token)))
          (helm-gtags--remove-carrige-returns)
          (unless (zerop status)
            (cond ((= status 1)
                   (error "Error: %s%s" (buffer-string) filename))
                  ((= status 3)
                   (error "Error: %s" (buffer-string)))
                  (t (error "%s: not found" token)))))))))

(defun helm-gtags--parse-file-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (zerop (process-file "global" nil t nil
                                 "--result=cscope" "-f" helm-gtags--parsed-file))
      (error "Failed: 'global --result=cscope -f %s" helm-gtags--parsed-file))
    (helm-gtags--remove-carrige-returns)))

(defun helm-gtags--push-context (context)
  (let* ((context-info (helm-gtags--get-or-create-context-info))
         (current-index (plist-get context-info :index))
         (context-stack (plist-get context-info :stack)))
    (unless (= current-index -1)
      (setq context-stack (nthcdr (1+ current-index) context-stack)))
    (setq helm-gtags--current-position nil)
    (push context context-stack)
    (helm-gtags--put-context-stack helm-gtags--tag-location -1 context-stack)))

(defsubst helm-gtags--select-find-file-func ()
  (if helm-gtags--use-otherwin
      #'helm-gtags--open-file-other-window
    #'helm-gtags--open-file))

(defun helm-gtags--do-open-file (open-func file line)
  (funcall open-func file helm-gtags-read-only)
  (goto-char (point-min))
  (forward-line (1- line))
  (back-to-indentation)
  (recenter)
  (helm-gtags--push-context helm-gtags--saved-context)
  (when helm-gtags-pulse-at-cursor
    (pulse-momentary-highlight-one-line (point))))

(defun helm-gtags--find-line-number (cand)
  (if (string-match "\\s-+\\([1-9][0-9]+\\)\\s-+" cand)
      (string-to-number (match-string-no-properties 1 cand))
    (error "Can't find line number in %s" cand)))

(defun helm-gtags--parse-file-action (cand)
  (let ((line (helm-gtags--find-line-number cand))
        (open-func (helm-gtags--select-find-file-func)))
    (helm-gtags--do-open-file open-func helm-gtags--parsed-file line)))

(defsubst helm-gtags--has-drive-letter-p (path)
  (string-match-p "\\`[a-zA-Z]:" path))

(defun helm-gtags--extract-file-and-line (cand)
  (if (and (helm-gtags--windows-p) (helm-gtags--has-drive-letter-p cand))
      (when (string-match "\\(\\`[a-zA-Z]:[^:]+\\):\\([^:]+\\)" cand)
        (cons (match-string-no-properties 1 cand)
              (string-to-number (match-string-no-properties 2 cand))))
    (let ((elems (split-string cand ":")))
      (cons (cl-first elems) (string-to-number (cl-second elems))))))

(defun helm-gtags--action-openfile (cand)
  (let* ((file-and-line (helm-gtags--extract-file-and-line cand))
         (filename (car file-and-line))
         (line (cdr file-and-line))
         (open-func (helm-gtags--select-find-file-func))
         (default-directory (helm-gtags--base-directory)))
    (helm-gtags--do-open-file open-func filename line)))

(defun helm-gtags--action-openfile-other-window (cand)
  (let ((helm-gtags--use-otherwin t))
    (helm-gtags--action-openfile cand)))

(defun helm-gtags--file-content-at-pos (file pos)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (format "%s:%d:%s:%s"
              file (line-number-at-pos)
              (helm-aif (which-function) (format "[%s]" it) "")
              (helm-current-line-contents)))))

(defun helm-gtags--files-candidate-transformer (file)
  (if (eq helm-gtags-path-style 'absolute)
      file
    (let ((removed-regexp (concat "\\`" helm-gtags--tag-location)))
      (replace-regexp-in-string removed-regexp "" file))))

(defun helm-gtags--show-stack-init ()
  (cl-loop with context-stack = (plist-get (helm-gtags--get-context-info) :stack)
           with stack-length = (length context-stack)
           for context in (reverse context-stack)
           for file = (plist-get context :file)
           for pos  = (plist-get context :position)
           for index = (1- stack-length) then (1- index)
           for line = (helm-gtags--file-content-at-pos file pos)
           for cand = (helm-gtags--files-candidate-transformer line)
           collect (cons cand (propertize cand 'index index))))

(defun helm-gtags--persistent-action (cand)
  (let* ((file-and-line (helm-gtags--extract-file-and-line cand))
         (filename (car file-and-line))
         (line (cdr file-and-line))
         (default-directory (helm-gtags--base-directory)))
    (when (eq helm-gtags-path-style 'relative)
      (setq helm-gtags--last-default-directory default-directory))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-highlight-current-line)))

(defvar helm-gtags--find-file-action
  (helm-make-actions
   "Open file" #'helm-gtags--action-openfile
   "Open file other window" #'helm-gtags--action-openfile-other-window))

(defvar helm-source-gtags-tags
  (helm-build-in-buffer-source "Jump to definitions"
    :init 'helm-gtags--tags-init
    :candidate-number-limit helm-gtags-maximum-candidates
    :real-to-display 'helm-gtags--candidate-transformer
    :persistent-action 'helm-gtags--persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action helm-gtags--find-file-action))

(defvar helm-source-gtags-pattern
  (helm-build-in-buffer-source "Find pattern"
    :init 'helm-gtags--pattern-init
    :candidate-number-limit helm-gtags-maximum-candidates
    :real-to-display 'helm-gtags--candidate-transformer
    :persistent-action 'helm-gtags--persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action helm-gtags--find-file-action))

(defvar helm-source-gtags-rtags
  (helm-build-in-buffer-source "Jump to references"
    :init 'helm-gtags--rtags-init
    :candidate-number-limit helm-gtags-maximum-candidates
    :real-to-display 'helm-gtags--candidate-transformer
    :persistent-action 'helm-gtags--persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action helm-gtags--find-file-action))

(defvar helm-source-gtags-gsyms
  (helm-build-in-buffer-source "Jump to symbols"
    :init 'helm-gtags--gsyms-init
    :candidate-number-limit helm-gtags-maximum-candidates
    :real-to-display 'helm-gtags--candidate-transformer
    :persistent-action 'helm-gtags--persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action helm-gtags--find-file-action))

(defun helm-gtags--highlight-candidate (candidate)
  (let ((regexp (concat "\\_<" helm-gtags--last-input "\\_>"))
        (limit (1- (length candidate)))
        (last-pos 0)
        (case-fold-search nil))
    (while (and (< last-pos limit)
                (string-match regexp candidate last-pos))
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'helm-gtags-match
                         candidate)
      (setq last-pos (1+ (match-end 0))))
    candidate))

(defun helm-gtags--transformer-regexp (candidate)
  (if (and (helm-gtags--windows-p) (helm-gtags--has-drive-letter-p candidate))
      "\\`\\([a-zA-Z]:[^:]+\\):\\([^:]+\\):\\(.*\\)"
    "\\`\\([^:]+\\):\\([^:]+\\):\\(.*\\)"))

(defun helm-gtags--candidate-transformer (candidate)
  (if (not helm-gtags-highlight-candidate)
      candidate
    (let ((regexp (helm-gtags--transformer-regexp candidate)))
      (when (string-match regexp candidate)
        (format "%s:%s:%s"
                (propertize (match-string 1 candidate) 'face 'helm-gtags-file)
                (propertize (match-string 2 candidate) 'face 'helm-gtags-lineno)
                (helm-gtags--highlight-candidate (match-string 3 candidate)))))))

(defun helm-gtags--parse-file-candidate-transformer (file)
  (let ((removed-file (replace-regexp-in-string "\\`\\S-+ " "" file)))
    (when (string-match "\\`\\(\\S-+\\) \\(\\S-+\\) \\(.+\\)\\'" removed-file)
      (format "%-25s %-5s %s"
              (match-string-no-properties 1 removed-file)
              (match-string-no-properties 2 removed-file)
              (match-string-no-properties 3 removed-file)))))

(defvar helm-source-gtags-find-tag-from-here
  (helm-build-in-buffer-source "Find tag from here"
    :init 'helm-gtags--find-tag-from-here-init
    :candidate-number-limit helm-gtags-maximum-candidates
    :real-to-display 'helm-gtags--candidate-transformer
    :persistent-action 'helm-gtags--persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action helm-gtags--find-file-action))

(defvar helm-source-gtags-parse-file
  (helm-build-in-buffer-source "Parse file"
    :init 'helm-gtags--parse-file-init
    :candidate-number-limit helm-gtags-maximum-candidates
    :real-to-display 'helm-gtags--parse-file-candidate-transformer
    :fuzzy-match helm-gtags-fuzzy-match
    :action 'helm-gtags--parse-file-action))

(defun helm-gtags--show-stack-action (cand)
  (let* ((index (get-text-property 0 'index cand))
         (context-info (helm-gtags--get-context-info))
         (context-stack (plist-get context-info :stack)))
    (helm-gtags--put-context-stack helm-gtags--tag-location
                                   index context-stack)
    (helm-gtags--move-to-context (nth index context-stack))))

(defvar helm-source-gtags-show-stack
  (helm-build-sync-source "Show Context Stack"
    :candidates 'helm-gtags--show-stack-init
    :volatile t
    :candidate-number-limit helm-gtags-maximum-candidates
    :persistent-action 'helm-gtags--persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action 'helm-gtags--show-stack-action))

;;;###autoload
(defun helm-gtags-select ()
  (interactive)
  (helm-gtags--common '(helm-source-gtags-select) nil))

;;;###autoload
(defun helm-gtags-select-path ()
  (interactive)
  (helm-gtags--common '(helm-source-gtags-select-path) nil))

(defsubst helm-gtags--beginning-of-defun ()
  (cl-case major-mode
    ((c-mode c++-mode java-mode) 'c-beginning-of-defun)
    (php-mode 'php-beginning-of-defun)
    (otherwise #'beginning-of-defun)))

(defsubst helm-gtags--end-of-defun ()
  (cl-case major-mode
    ((c-mode c++-mode java-mode malabar-mode) 'c-end-of-defun)
    (php-mode 'php-end-of-defun)
    (otherwise #'end-of-defun)))

(defun helm-gtags--current-funcion-bound ()
  (save-excursion
    (let (start)
      (funcall (helm-gtags--beginning-of-defun))
      (setq start (line-number-at-pos))
      (funcall (helm-gtags--end-of-defun))
      (cons start (line-number-at-pos)))))

(defun helm-gtags--tags-refered-from-this-function ()
  (let* ((file (helm-gtags--real-file-name))
         (bound (helm-gtags--current-funcion-bound))
         (start-line (car bound))
         (end-line (cdr bound)))
    (with-temp-buffer
      (unless (process-file "global" nil t nil "-f" "-r" file)
        (error "Failed: global -f -r %s" file))
      (goto-char (point-min))
      (let (tagnames finish)
        (while (and (not finish) (not (eobp)))
          (let* ((cols (split-string (helm-current-line-contents) nil t))
                 (lineno (string-to-number (cl-second cols))))
            (if (and (> lineno start-line) (< lineno end-line))
                (let* ((tag (cl-first cols))
                       (elm (cl-find tag tagnames :test 'equal)))
                  (unless elm
                    (push tag tagnames)))
              (when (>= lineno end-line)
                (setq finish t)))
            (forward-line 1)))
        (reverse tagnames)))))

(defun helm-gtags--tag-in-function-persistent-action (cand)
  (let* ((bound (helm-gtags--current-funcion-bound))
         (limit (save-excursion
                  (goto-char (point-min))
                  (forward-line (cdr bound))
                  (goto-char (line-end-position))
                  (point))))
    (when (search-forward cand nil limit)
      (helm-highlight-current-line))))

;;;###autoload
(defun helm-gtags-tags-in-this-function ()
  "Show tagnames which are referenced in this function and jump to it."
  (interactive)
  (let ((tags (helm-gtags--tags-refered-from-this-function)))
    (unless tags
      (error "There are no tags which are refered from this function."))
    (let* ((name (format "Tags in [%s]" (which-function)))
           (tag (helm-comp-read
                 "Tagnames: " tags
                 :must-match t :name name
                 :persistent-action 'helm-gtags--tag-in-function-persistent-action)))
      (helm-gtags-find-tag tag))))

(defun helm-gtags--source-select-tag (candidate)
  (helm-build-in-buffer-source "Select Tag"
    :init (lambda () (helm-gtags--tags-init candidate))
    :candidate-number-limit helm-gtags-maximum-candidates
    :persistent-action 'helm-gtags--persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action helm-gtags--find-file-action))

(defun helm-gtags--source-select-rtag (candidate)
  (helm-build-in-buffer-source "Select Rtag"
    :init (lambda () (helm-gtags--rtags-init candidate))
    :candidate-number-limit helm-gtags-maximum-candidates
    :persistent-action 'helm-gtags--persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action helm-gtags--find-file-action))

(defsubst helm-gtags--action-by-timer (src)
  (run-with-timer 0.1 nil (lambda () (helm-gtags--common (list src) nil))))

(defun helm-gtags--select-tag-action (c)
  (helm-gtags--action-by-timer (helm-gtags--source-select-tag c)))

(defun helm-gtags--select-rtag-action (c)
  (helm-gtags--action-by-timer (helm-gtags--source-select-rtag c)))

(defun helm-gtags--select-cache-init-common (args tagfile)
  (let ((cache (helm-gtags--get-result-cache tagfile)))
    (if cache
        (insert cache)
      (apply #'process-file "global" nil t nil args)
      (let* ((cache (buffer-string))
             (cache-size (length cache)))
        (when (<= cache-size helm-gtags-cache-max-result-size)
          (helm-gtags--put-result-cache tagfile cache))))))

(defun helm-gtags--source-select-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (if (not helm-gtags-cache-select-result)
        (progn
          (process-file "global" nil t nil "-c")
          (helm-gtags--remove-carrige-returns))
      (helm-gtags--select-cache-init-common '("-c") "GTAGS"))))

(defvar helm-source-gtags-select
  (helm-build-in-buffer-source "Find tag from here"
    :init 'helm-gtags--source-select-init
    :candidate-number-limit helm-gtags-maximum-candidates
    :persistent-action #'ignore
    :fuzzy-match helm-gtags-fuzzy-match
    :action (helm-make-actions
             "Goto the location" #'helm-gtags--select-tag-action
             "Goto the location(other buffer)"
             (lambda (c)
               (setq helm-gtags--use-otherwin t)
               (helm-gtags--select-tag-action c))
             "Move to the referenced point" #'helm-gtags--select-rtag-action)))

(defun helm-gtags--select-path-init ()
  (helm-gtags--find-tag-directory)
  (with-current-buffer (helm-candidate-buffer 'global)
    (let ((options (if (eq helm-gtags-path-style 'relative) "-Po" "-Poa")))
      (if (not helm-gtags-cache-select-result)
          (progn
            (process-file "global" nil t nil options)
            (helm-gtags--remove-carrige-returns))
        (helm-gtags--select-cache-init-common (list options) "GPATH")))))

(defun helm-gtags--file-name (name)
  (let ((remote (file-remote-p default-directory)))
    (if (not remote)
        name
      (cl-case helm-gtags-path-style
        (relative name)
        (otherwise (concat remote name))))))

(defun helm-gtags--find-file-common (open-fn cand)
  (let ((default-directory (helm-gtags--base-directory)))
    (funcall open-fn (helm-gtags--file-name cand))))

(defun helm-gtags--find-file (cand)
  (helm-gtags--find-file-common #'find-file cand))

(defun helm-gtags--find-file-other-window (cand)
  (helm-gtags--find-file-common #'find-file-other-window cand))

(defvar helm-gtags--file-util-action
  (helm-make-actions
   "Open file" #'helm-gtags--find-file
   "Open file other window" #'helm-gtags--find-file-other-window))

(defun helm-gtags--file-persistent-action (cand)
  (let ((default-directory (with-helm-current-buffer
                             default-directory)))
    (helm-ff-kill-or-find-buffer-fname (helm-gtags--file-name cand))))

(defvar helm-source-gtags-select-path
  (helm-build-in-buffer-source "Select path"
    :init 'helm-gtags--select-path-init
    :candidate-number-limit helm-gtags-maximum-candidates
    :real-to-display 'helm-gtags--files-candidate-transformer
    :persistent-action #'helm-gtags--file-persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action helm-gtags--file-util-action))

(defun helm-gtags--searched-directory ()
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 (let ((dir (read-directory-name "Input Directory: ")))
         (setq helm-gtags--local-directory (file-name-as-directory dir))))
    (16 (file-name-directory (buffer-file-name)))))

(defsubst helm-gtags--using-other-window-p ()
  (< (prefix-numeric-value current-prefix-arg) 0))

(defun helm-gtags--make-gtags-sentinel (action)
  (lambda (process _event)
    (when (eq (process-status process) 'exit)
      (if (zerop (process-exit-status process))
          (message "Success: %s TAGS" action)
        (message "Failed: %s TAGS(%d)" action (process-exit-status process))))))

(defsubst helm-gtags--read-gtagslabel ()
  (let ((labels '("default" "native" "ctags" "new-ctags" "pygments")))
    (completing-read "GTAGSLABEL(Default: default): " labels nil t nil nil "default")))

(defsubst helm-gtags--label-option (label)
  (concat "--gtagslabel=" label))

;;;###autoload
(defun helm-gtags-create-tags (dir label)
  (interactive
   (list (read-directory-name "Root Directory: ")
         (helm-gtags--read-gtagslabel)))
  (let ((default-directory dir)
        (proc-buf (get-buffer-create " *helm-gtags-create*")))
    (let ((proc (start-file-process "helm-gtags-create" proc-buf
                                    "gtags" "-q" (helm-gtags--label-option label))))
      (set-process-sentinel proc (helm-gtags--make-gtags-sentinel 'create)))))

(defun helm-gtags--find-tag-simple ()
  (or (getenv "GTAGSROOT")
      (locate-dominating-file default-directory "GTAGS")
      (if (not (yes-or-no-p "File GTAGS not found. Run 'gtags'? "))
          (user-error "Abort")
        (let* ((tagroot (read-directory-name "Root Directory: "))
               (label (helm-gtags--read-gtagslabel))
               (default-directory tagroot))
          (message "gtags is generating tags....")
          (let ((label-opt (helm-gtags--label-option label)))
            (unless (zerop (process-file "gtags" nil nil nil "-q" label-opt))
              (error "Failed: 'gtags -q %s'" label-opt)))
          tagroot))))

(defun helm-gtags--current-file-and-line ()
  (let* ((buffile (buffer-file-name))
         (path (cl-case helm-gtags-path-style
                 (absolute buffile)
                 (root
                  (file-relative-name buffile (helm-gtags--find-tag-directory)))
                 (relative
                  (file-relative-name buffile (helm-gtags--base-directory))))))
    (format "%s:%d" path (line-number-at-pos))))

(defsubst helm-gtags--clear-variables ()
  (setq helm-gtags--last-default-directory nil))

(defun helm-gtags--common (srcs tagname)
  (helm-gtags--clear-variables)
  (let ((helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t)
        (dir (helm-gtags--searched-directory))
        (src (car srcs))
        (preselect-regexp (when helm-gtags-preselect
                            (regexp-quote (helm-gtags--current-file-and-line)))))
    (when (symbolp src)
      (setq src (symbol-value src)))
    (unless helm-gtags--use-otherwin
      (setq helm-gtags--use-otherwin (helm-gtags--using-other-window-p)))
    (when tagname
      (setq helm-gtags--query tagname))
    (let ((tagroot (helm-gtags--find-tag-simple)))
      (helm-attrset 'helm-gtags-base-directory dir src)
      (when tagname
        (helm-attrset 'name (format "%s in %s" tagname (or dir tagroot)) src))
      (helm :sources srcs :buffer helm-gtags--buffer
            :preselect preselect-regexp))))

;;;###autoload
(defun helm-gtags-delete-tags ()
  "Delete file GTAGS, GRTAGS, GPATH, ID etc. generated by gtags."
  (interactive)
  (let* ((root-dir (helm-gtags--tag-directory))
         (re (concat "\\`" (regexp-opt '("GPATH" "GRTAGS" "GTAGS" "ID")) "\\'"))
         (files (cl-remove-if-not
                 (lambda (file)
                   ;; Don't trust `directory-files'.
                   (let ((case-fold-search nil))
                     (string-match-p re (file-name-nondirectory file))))
                 (directory-files root-dir t re)))
         (buffer "*GTags File List*"))
    (unless files
      (user-error "No tag files found"))
    (with-output-to-temp-buffer buffer
      (princ (mapconcat #'identity files "\n")))
    (let ((win (get-buffer-window buffer)))
      (unwind-protect
          (progn
            (fit-window-to-buffer win)
            (when (yes-or-no-p "Remove GNU Global tag files? ")
              (with-demoted-errors (mapc #'delete-file files))
              ))
        (when (window-live-p win)
          (quit-window t win))))))

;;;###autoload
(defun helm-gtags-find-tag (tag)
  "Jump to definition"
  (interactive
   (list (helm-gtags--read-tagname 'tag)))
  (helm-gtags--common '(helm-source-gtags-tags) tag))

;;;###autoload
(defun helm-gtags-find-tag-other-window (tag)
  "Jump to definition in other window."
  (interactive
   (list (helm-gtags--read-tagname 'tag)))
  (setq helm-gtags--use-otherwin t)
  (helm-gtags-find-tag tag))

;;;###autoload
(defun helm-gtags-find-rtag (tag)
  "Jump to referenced point"
  (interactive
   (list (helm-gtags--read-tagname 'rtag (which-function))))
  (helm-gtags--common '(helm-source-gtags-rtags) tag))

;;;###autoload
(defun helm-gtags-find-symbol (tag)
  "Jump to the symbol location"
  (interactive
   (list (helm-gtags--read-tagname 'symbol)))
  (helm-gtags--common '(helm-source-gtags-gsyms) tag))

;;;###autoload
(defun helm-gtags-find-pattern (pattern)
  "Grep and jump by gtags tag files."
  (interactive
   (list (helm-gtags--read-tagname 'pattern)))
  (helm-gtags--common '(helm-source-gtags-pattern) pattern))

(defun helm-gtags--find-file-after-hook ()
  (helm-gtags--push-context helm-gtags--saved-context))

(defvar helm-source-gtags-files
  (helm-build-in-buffer-source "Find files"
    :init #'helm-gtags--files-init
    :candidate-number-limit helm-gtags-maximum-candidates
    :real-to-display #'helm-gtags--files-candidate-transformer
    :persistent-action #'helm-gtags--file-persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action helm-gtags--file-util-action))

;;;###autoload
(defun helm-gtags-find-files (file)
  "Find file from tagged with gnu global."
  (interactive
   (list (helm-gtags--read-tagname 'find-file)))
  (add-hook 'helm-after-action-hook 'helm-gtags--find-file-after-hook)
  (unwind-protect
      (helm-gtags--common '(helm-source-gtags-files) file)
    (remove-hook 'helm-after-action-hook 'helm-gtags--find-file-after-hook)))

;;;###autoload
(defun helm-gtags-find-tag-from-here ()
  "Jump point by current point information.
Jump to definition point if cursor is on its reference.
Jump to reference point if curosr is on its definition"
  (interactive)
  (helm-gtags--common '(helm-source-gtags-find-tag-from-here) nil))

;;;###autoload
(defun helm-gtags-dwim ()
  "Find by context. Here is
- on include statement then jump to included file
- on symbol definition then jump to its references
- on reference point then jump to its definition."
  (interactive)
  (let ((line (helm-current-line-contents)))
    (if (string-match helm-gtags--include-regexp line)
        (let ((helm-gtags-use-input-at-cursor t))
          (helm-gtags-find-files (match-string-no-properties 1 line)))
      (if (and (buffer-file-name) (thing-at-point 'symbol))
          (helm-gtags-find-tag-from-here)
        (call-interactively 'helm-gtags-find-tag)))))

(defun helm-gtags--set-parsed-file ()
  (let* ((this-file (file-name-nondirectory (buffer-file-name)))
         (file (if current-prefix-arg
                   (read-file-name "Parsed File: " nil this-file)
                 this-file)))
    (setq helm-gtags--parsed-file (expand-file-name file))))

(defun helm-gtags--find-preselect-line ()
  (let ((defun-bound (bounds-of-thing-at-point 'defun)))
    (if (not defun-bound)
        (line-number-at-pos)
      (let ((defun-begin-line (line-number-at-pos (car defun-bound)))
            (filename (helm-gtags--real-file-name)))
        (with-temp-buffer
          (unless (zerop (process-file "global" nil t nil "-f" filename))
            (error "Failed: global -f"))
          (goto-char (point-min))
          (let (start-line)
            (while (and (not start-line)
                        (re-search-forward "^\\S-+\\s-+\\([1-9][0-9]*\\)" nil t))
              (let ((line (string-to-number (match-string-no-properties 1))))
                (when (>= line defun-begin-line)
                  (setq start-line line))))
            (or start-line (line-number-at-pos))))))))

;;;###autoload
(defun helm-gtags-parse-file ()
  "Parse current file with gnu global. This is similar to `imenu'.
You can jump definitions of functions, symbols in this file."
  (interactive)
  (helm-gtags--find-tag-directory)
  (helm-gtags--save-current-context)
  (setq helm-gtags--use-otherwin (helm-gtags--using-other-window-p))
  (helm-gtags--set-parsed-file)
  (helm-attrset 'name
                (format "Parsed File: %s"
                        (file-relative-name helm-gtags--parsed-file
                                            helm-gtags--tag-location))
                helm-source-gtags-parse-file)
  (let ((presel (when helm-gtags-preselect
                  (format "^\\S-+\\s-+%d\\s-+" (helm-gtags--find-preselect-line)))))
    (helm :sources '(helm-source-gtags-parse-file)
          :buffer helm-gtags--buffer :preselect presel)))

;;;###autoload
(defun helm-gtags-pop-stack ()
  "Jump to previous point on the context stack and pop it from stack."
  (interactive)
  (let* ((context-info (helm-gtags--get-context-info))
         (context-stack (plist-get context-info :stack))
         (context (pop context-stack)))
    (helm-gtags--put-context-stack helm-gtags--tag-location -1 context-stack)
    (helm-gtags--move-to-context context)))

;;;###autoload
(defun helm-gtags-show-stack ()
  "Show current context stack."
  (interactive)
  (helm-other-buffer 'helm-source-gtags-show-stack
                     (get-buffer-create helm-gtags--buffer)))

;;;###autoload
(defun helm-gtags-clear-stack ()
  "Clear current context stack."
  (interactive)
  (let ((tag-location (helm-gtags--find-tag-directory)))
    (message "Clear '%s' context stack." tag-location)
    (remhash tag-location helm-gtags--context-stack)))

;;;###autoload
(defun helm-gtags-clear-all-stacks ()
  "Clear all context stacks."
  (interactive)
  (message "Clear all context statks.")
  (setq helm-gtags--context-stack (make-hash-table :test 'equal)))

(defun helm-gtags--read-tag-directory ()
  (let ((dir (read-directory-name "Directory tag generated: " nil nil t)))
    ;; On Windows, "gtags d:/tmp" work, but "gtags d:/tmp/" doesn't
    (directory-file-name (expand-file-name dir))))

(defsubst helm-gtags--how-to-update-tags ()
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 'entire-update)
    (16 'generate-other-directory)
    (otherwise 'single-update)))

(defun helm-gtags--update-tags-command (how-to)
  (cl-case how-to
    (entire-update '("global" "-u"))
    (generate-other-directory (list "gtags" (helm-gtags--read-tag-directory)))
    (single-update (list "global" "--single-update" (helm-gtags--real-file-name)))))

(defun helm-gtags--update-tags-p (how-to interactive-p current-time)
  (or interactive-p
      (and (eq how-to 'single-update)
           (buffer-file-name)
           (or (not helm-gtags-update-interval-second)
               (>= (- current-time helm-gtags--last-update-time)
                   helm-gtags-update-interval-second)))))

;;;###autoload
(defun helm-gtags-update-tags ()
  "Update TAG file. Update All files with `C-u' prefix.
Generate new TAG file in selected directory with `C-u C-u'"
  (interactive)
  (let ((how-to (helm-gtags--how-to-update-tags))
        (interactive-p (called-interactively-p 'interactive))
        (current-time (float-time (current-time))))
    (when (helm-gtags--update-tags-p how-to interactive-p current-time)
      (let* ((cmds (helm-gtags--update-tags-command how-to))
             (proc (apply #'start-file-process "helm-gtags-update-tag" nil cmds)))
        (if (not proc)
            (message "Failed: %s" (string-join cmds " "))
          (set-process-sentinel proc (helm-gtags--make-gtags-sentinel 'update))
          (setq helm-gtags--last-update-time current-time))))))

;;;###autoload
(defun helm-gtags-resume ()
  "Resurrect previously invoked `helm-gtags` command."
  (interactive)
  (unless (get-buffer helm-gtags--buffer)
    (error "Error: helm-gtags buffer is not existed."))
  (helm-resume helm-gtags--buffer))

(defsubst helm-gtags--check-browser-installed (browser)
  (let ((used-browser (or browser "mozilla")))
    (unless (executable-find used-browser)
      (error "Not found browser '%s'" used-browser))))

(defun helm-gtags-display-browser ()
  "Display current screen on hypertext browser.
`browse-url-generic-program' is used as browser if its value is non-nil.
`mozilla' is used in other case."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not file)
        (error "This buffer is not related to file.")
      (let* ((lineopt (concat "+" (number-to-string (line-number-at-pos))))
             (browser (symbol-value 'browse-url-generic-program))
             (args (list lineopt file)))
        (helm-gtags--check-browser-installed browser)
        (when browser
          (setq args (append (list "-b" browser) args)))
        ;; `gozilla' commend never returns error status if command is failed.
        (apply #'process-file "gozilla" nil nil nil args)))))

(defvar helm-gtags-mode-name " HelmGtags")
(defvar helm-gtags-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-gtags-mode ()
  "Enable helm-gtags"
  :init-value nil
  :global     nil
  :keymap     helm-gtags-mode-map
  :lighter    helm-gtags-mode-name
  (if helm-gtags-mode
      (when helm-gtags-auto-update
        (add-hook 'after-save-hook 'helm-gtags-update-tags nil t))
    (when helm-gtags-auto-update
      (remove-hook 'after-save-hook 'helm-gtags-update-tags t))))

;; Key mapping of gtags-mode.
(when helm-gtags-suggested-key-mapping
  ;; Current key mapping.
  (let ((command-table '(("h" . helm-gtags-display-browser)
                         ("P" . helm-gtags-find-files)
                         ("f" . helm-gtags-parse-file)
                         ("g" . helm-gtags-find-pattern)
                         ("s" . helm-gtags-find-symbol)
                         ("r" . helm-gtags-find-rtag)
                         ("t" . helm-gtags-find-tag)
                         ("d" . helm-gtags-find-tag)))
        (key-func (if (string-prefix-p "\\" helm-gtags-prefix-key)
                      #'concat
                    (lambda (prefix key) (kbd (concat prefix " " key))))))
    (cl-loop for (key . command) in command-table
             do
             (define-key helm-gtags-mode-map (funcall key-func helm-gtags-prefix-key key) command))

    ;; common
    (define-key helm-gtags-mode-map "\C-]" 'helm-gtags-find-tag-from-here)
    (define-key helm-gtags-mode-map "\C-t" 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map "\e*" 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map "\e." 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map "\C-x4." 'helm-gtags-find-tag-other-window)))

(provide 'helm-gtags)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags.el ends here
