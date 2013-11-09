;;; helm-gtags.el --- GNU GLOBAL helm interface

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags
;; 纪秀峰 fork version
;; URL: https://github.com/jixiuf/emacs-helm-gtags
;; Author: 纪秀峰 <jixiuf@gmail.com>
;; Version: 2.0
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
;;                  (add-to-list 'helm-gtags-tag-location-list "/usr/include/")
;;                  (local-set-key (kbd "M-.") 'helm-gtags-find-tag-and-symbol)
;;                  (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
;;                  (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
;;                  (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
;;                  (local-set-key (kbd "C-,") 'helm-gtags-pop-stack)
;;                  (local-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)
;;                  (local-set-key (kbd "C-c C-f") 'helm-gtags-find-files)))
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'helm)
(require 'helm-files)
(require 'helm-match-plugin)
(require 'which-func)
(require 'thingatpt)

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

(defcustom helm-gtags-auto-update nil
  "*If non-nil, tag files are updated whenever a file is saved."
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-tag-location-list nil
  "tag locations."
  :type 'list
  :group 'helm-gtags)

(defcustom helm-gtags-default-candidate-limit 9999
  "default candidate limit."
  :type 'integer
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
(defvar helm-gtags-context-stack nil)
(defvar helm-gtags-saved-context nil)
(defvar helm-gtags-use-otherwin nil)
(defvar helm-gtags-local-directory nil)
(defvar helm-gtags-parsed-file nil)
(defvar helm-gtags-tag-cache nil)
(defvar helm-gtags-symbol-cache nil)

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
    (thing-at-point 'symbol)
    ))

(defsubst helm-gtags-type-is-not-file-p (type)
  (not (eq type :file)))

;; (defun helm-gtags-input (type)
;;   (let ((tagname (helm-gtags-token-at-point))
;;         (prompt (assoc-default type helm-gtags-prompt-alist))
;;         (comp-func (assoc-default type helm-gtags-comp-func-alist)))
;;     (when (and tagname (helm-gtags-type-is-not-file-p type))
;;       (setq prompt (format "%s(default \"%s\") " prompt tagname)))
;;     (let ((completion-ignore-case helm-gtags-ignore-case)
;;           (completing-read-function 'completing-read-default))
;;       (completing-read prompt comp-func nil nil nil
;;                        'helm-gtags-completing-history tagname))))

(defun helm-gtags-find-tag-directory()
  (with-temp-buffer
    (let ((status (call-process-shell-command "global -p" nil t)))
      (unless (zerop status)
        (error "GTAGS not found"))
      (goto-char (point-min))
      (let ((tagroot (buffer-substring
                      (point) (line-end-position))))
        (setq helm-gtags-tag-location (file-name-as-directory tagroot))))))

(defun helm-gtags-base-directory ()
  ;; (message helm-gtags-local-directory)
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

(defun helm-gtags-pop-context ()
  (unless helm-gtags-context-stack
    (error "Context stack is empty." ))
  (let* ((context (pop helm-gtags-context-stack))
         (file (plist-get context :file))
         (curpoint (plist-get context :position))
         (readonly (plist-get context :readonly)))
    (helm-gtags-open-file file readonly)
    (goto-char curpoint)))

(defun get-helm-first-patern()
  (car (helm-mp-split-pattern helm-pattern))
  )
(defun read-lines-from-buf(buf limit)
  (with-current-buffer buf
    (goto-char (point-min))
    (let (candidates (i 0))
      (while (or (and (not (eobp)) (null limit)) (and limit (< i limit)))
        (add-to-list 'candidates (buffer-substring (line-beginning-position) (line-end-position)))
        (forward-line)
        (setq i (1+ i)))
      candidates
      )))

(defun helm-gtags-exec-global-command (type &optional input)
  ;; (helm-gtags-find-tag-directory)
  (let (cmd candidates
            (dirs (helm-attr 'helm-gtags-tag-location-list (helm-get-current-source)))
            (default-tag-dir (helm-gtags-searched-directory))
                                        ; (helm-gtags-input type)

            (buf-coding buffer-file-coding-system)
            )
    (when default-tag-dir (add-to-list 'dirs default-tag-dir ))
    (with-current-buffer helm-current-buffer (helm-gtags-save-current-context))
    (with-temp-buffer
      (let (begin end
                  (default-directory default-directory)
                  (coding-system-for-read buf-coding)
                  (coding-system-for-write buf-coding)
                  )
        (dolist (dir dirs)
          (setq cmd (helm-gtags-construct-command type dir input))
          (setq default-directory (helm-gtags-base-directory))
          ;; (print cmd)
          (goto-char (point-max))
          (setq begin (point))
          (call-process-shell-command cmd nil t)
          (setq end (point))
          (put-text-property begin end 'default-directory default-directory)
          )
        (setq candidates (read-lines-from-buf (current-buffer) helm-gtags-default-candidate-limit))
        (case type
          (:tag    (setq helm-gtags-tag-cache (cons input candidates)))
          (:symbol (setq helm-gtags-symbol-cache (cons input candidates)))
          )))
    candidates))

(defun helm-gtags-find-tag-from-here-candidates()
  (let (cmd candidates
            token begin end
            (dirs (helm-attr 'helm-gtags-tag-location-list (helm-get-current-source)))
            (default-directory (helm-gtags-searched-directory))
            (buf-filename  (format "\"%s\"" (buffer-file-name helm-current-buffer)))
            )
    (setq helm-gtags-local-directory nil)
    (helm-gtags-save-current-context)
    (with-temp-buffer
      (setq cmd
            (with-current-buffer helm-current-buffer
              (setq token (helm-gtags-token-at-point))
              (format "global --result=grep --from-here=%d:%s %s"
                      (line-number-at-pos)
                      buf-filename
                      token)))
      (print cmd)
      (goto-char (point-max))
      ;; (setq begin (point))
      (call-process-shell-command cmd nil t)
      ;; (setq end (point))
      ;; (put-text-property begin end 'default-directory default-directory)
      (setq candidates (read-lines-from-buf (current-buffer) helm-gtags-default-candidate-limit)))
    candidates
    )
  )


(defun helm-gtags-candidates-in-buffer(type &optional in)
  (helm-gtags-exec-global-command type in)
  )
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

(defun helm-gtags-construct-command (type dir &optional in)
  (setq helm-gtags-local-directory nil)
  ;; (when (and dir (helm-gtags-type-is-not-file-p type))
  (when dir
    (setq helm-gtags-local-directory dir))
  (let ((input (or in (car (helm-mp-split-pattern helm-pattern)))) ;(or in (helm-gtags-input type))
        (option (helm-gtags-construct-option type)))
    (when (and (string= input "") (helm-gtags-type-is-not-file-p type))
      (error "Input is empty!!"))
    (format "global %s %s" option input)))

(defun helm-gtags-parse-file-candidates ()
  (let ((cmd (format "global --result cscope -f \"%s\"" helm-gtags-parsed-file))
        candidates)
    (with-temp-buffer
      (if (zerop (call-process-shell-command cmd nil t))
          (setq candidates (read-lines-from-buf (current-buffer)
                                                helm-gtags-default-candidate-limit))
        (error "Failed: %s" cmd)))
    candidates))

(defun helm-gtags-push-context (context)
  (unless(member context helm-gtags-context-stack)
    (push  context helm-gtags-context-stack)
    (print helm-gtags-context-stack)
    ))

(defun helm-gtags-select-find-file-func ()
  (if helm-gtags-use-otherwin
      'helm-gtags-open-file-other-window
    'helm-gtags-open-file))

(defun helm-gtags-do-open-file (open-func file line)
  (funcall open-func file helm-gtags-read-only)
  (goto-char (point-min))
  (forward-line (1- line))
  (helm-gtags-push-context helm-gtags-saved-context))

(defun helm-gtags-parse-file-action (cand)
  (let ((line (when (string-match "\\s-+\\([1-9][0-9]*\\)\\s-+" cand)
                (string-to-number (match-string 1 cand))))
        (open-func (helm-gtags-select-find-file-func)))
    (helm-gtags-do-open-file open-func helm-gtags-parsed-file line)))

(defun helm-gtags-action-openfile (_elm)
  (let* ((elm (helm-get-selection nil 'withprop))
         (elems (split-string elm ":"))
         (filename (first elems))
         (line (string-to-number (second elems)))
         (open-func (helm-gtags-select-find-file-func))
         (default-directory (or (get-text-property 0 'default-directory elm) (helm-gtags-base-directory))))
    ;; (message default-directory)
    (helm-gtags-do-open-file open-func filename line)))

(defun helm-gtags-file-content-at-pos (file pos)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (let ((curfunc (which-function))
            (line (line-number-at-pos))
            (content (or (buffer-substring
                          (line-beginning-position) (line-end-position))
                         "")))
        (format "%s:%d%s:%s\n"
                file line
                (helm-aif curfunc (format "[%s]" it) "")
                content)))))

(defun helm-gtags-show-stack-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (loop for context in (reverse helm-gtags-context-stack)
          for file = (plist-get context :file)
          for pos  = (plist-get context :position)
          do
          (insert (helm-gtags-file-content-at-pos file pos)))))

(defun helm-gtags-tags-persistent-action (_cand)
  (let* ((elems (split-string (helm-get-selection nil 'withprop) ":"))
         (filename (first elems))
         (line (string-to-number (second elems)))
         (default-directory (helm-gtags-base-directory)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-match-line-color-current-line)))

(defun helm-gtags-candidates-in-buffer-tag(&optional in)
  (let ((input (or in (car (helm-mp-split-pattern helm-pattern)))))
    (if (and helm-gtags-tag-cache (string-equal input (car helm-gtags-tag-cache)))
        (progn
          (with-current-buffer helm-current-buffer (helm-gtags-save-current-context))
          ;; (with-helm-buffer (helm-gtags-save-current-context))
          (cdr helm-gtags-tag-cache))
      (helm-gtags-candidates-in-buffer :tag input)
      )))

(defun helm-gtags-candidates-in-buffer-symbol(&optional in)
  (let ((input (or in (car (helm-mp-split-pattern helm-pattern)))))
    (if (and helm-gtags-symbol-cache (string-equal input (car helm-gtags-symbol-cache)))
        (progn
          (with-current-buffer helm-current-buffer (helm-gtags-save-current-context))
          ;; (with-helm-buffer (helm-gtags-save-current-context))
          (cdr helm-gtags-symbol-cache))
      (helm-gtags-candidates-in-buffer :symbol input)
      )))

(defun helm-gtags-candidates-in-buffer-rtag(&optional in)
  (helm-gtags-candidates-in-buffer :rtag (or in (car (helm-mp-split-pattern helm-pattern)))))

(defun helm-gtags-candidates-in-buffer-files(&optional in)
  (let ((input (or in (car (helm-mp-split-pattern helm-pattern)))))
    (helm-gtags-candidates-in-buffer :file input)))

(defvar helm-source-gtags-tags
  '((name . "tag")
    (candidates . helm-gtags-candidates-in-buffer-tag)
    (volatile);;candidates
    (delayed)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-gsyms
  '((name . "symbol")
    (candidates . helm-gtags-candidates-in-buffer-symbol)
    (volatile);;candidates
    (delayed)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-rtags
  '((name . "rtags")
    (candidates . helm-gtags-candidates-in-buffer-rtag)
    (volatile);;candidates
    (delayed)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile))
  )

(defun helm-gtags-files-candidate-transformer (file)
  (let ((removed-regexp (format "^%s" helm-gtags-tag-location)))
    (replace-regexp-in-string removed-regexp "" file)))

(defun helm-gtags-parse-file-candidate-transformer (file)
  (let ((removed-file (replace-regexp-in-string "\\`\\S-+ " "" file)))
    (when (string-match "\\`\\(\\S-+\\) \\(\\S-+\\) \\(.+\\)\\'" removed-file)
      (format "%-25s %-5s %s"
              (match-string 1 removed-file)
              (match-string 2 removed-file)
              (match-string 3 removed-file)))))

(defvar helm-source-gtags-files
  `((name . "gnu global files")
    (candidates . helm-gtags-candidates-in-buffer-files)
    ;; (volatile);;candidates
    (real-to-display . helm-gtags-files-candidate-transformer)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)
    (type . file)))

(defvar helm-source-gtags-find-tag-from-here
  '((name . "tags")
    (candidates . helm-gtags-find-tag-from-here-candidates)
    ;; (volatile) ;;candidates
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile))
  )

(defvar helm-source-gtags-parse-file
  `((name . "Parsed File")
    (candidates . helm-gtags-parse-file-candidates)
    ;; (candidates-in-buffer)
    ;; (get-line . buffer-substring)
    (real-to-display . helm-gtags-parse-file-candidate-transformer)
    (action . helm-gtags-parse-file-action)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)))

(defvar helm-source-gtags-show-stack
  `((name . "Show Context Stack")
    (init . helm-gtags-show-stack-init)
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (real-to-display . helm-gtags-files-candidate-transformer)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

;;;###autoload
(defun helm-gtags-select ()
  (interactive)
  (helm-gtags-common '(helm-source-gtags-select) "") )

(defun helm-source-gtags-select-tag (candidate)
  `((name . "tags")
    (candidates .  (lambda ()
                     (helm-gtags-candidates-in-buffer-tag ,candidate)) )
    (volatile);;candidates
    (delayed)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile))
  )


(defun helm-source-gtags-select-rtag (candidate)
  `((name . "rtags")
    (candidates . helm-gtags-candidates-in-buffer-rtag)
    (volatile);;candidates
    (delayed)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile))
  )

(defun helm-source-gtags-select-tag-action (c)
  (helm-run-after-quit
   `(lambda ()
      (helm-gtags-common (list (helm-source-gtags-select-tag ,c)) ,c))))

(defun helm-source-gtags-select-rtag-action (c)
  (helm-run-after-quit
   `(lambda ()
      (helm-gtags-common (list (helm-source-gtags-select-rtag ,c)) ,c))))

(defun helm-source-gtags-select-init()
  (let (candidates
        (cmd "global -c")
        (dirs (helm-attr 'helm-gtags-tag-location-list (helm-get-current-source)))
        (default-tag-dir (helm-gtags-searched-directory))
        (buf-coding buffer-file-coding-system)
        )
    (when default-tag-dir (add-to-list 'dirs default-tag-dir ))
    (with-current-buffer (helm-candidate-buffer 'global)
      (let (begin end
                  (default-directory default-directory)
                  (coding-system-for-read buf-coding)
                  (coding-system-for-write buf-coding)
                  )
        (dolist (dir dirs)
          (setq default-directory (helm-gtags-base-directory))
          (goto-char (point-max))
          (setq begin (point))
          (call-process-shell-command cmd nil t)
          (setq end (point))
          (put-text-property begin end 'default-directory default-directory)
          )))))

(defvar helm-source-gtags-select
  `((name . "GNU GLOBAL SELECT")
    (init . helm-source-gtags-select-init)
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)
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
    (16 (file-name-directory (buffer-file-name)))
    (t (file-name-directory (helm-gtags-find-tag-directory)))
    ))

(defsubst helm-gtags--using-other-window-p ()
  (< (prefix-numeric-value current-prefix-arg) 0))

(defun helm-gtags-common (srcs &optional input)
  (let (
        ;; (helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t)
        (buf (get-buffer-create helm-gtags-buffer))
        (dir (helm-gtags-searched-directory))
        (custom-dirs (mapcar (lambda(Dir) (file-name-as-directory Dir))
                             helm-gtags-tag-location-list))
        (src (car srcs)))
    (when dir (add-to-list 'custom-dirs dir))
    (when (helm-gtags--using-other-window-p) (setq helm-gtags-use-otherwin t))
    (dolist (src srcs)
      (when (symbolp src) (setq src (symbol-value src)))
      (unless (helm-attr 'init-name src) (helm-attrset 'init-name  (helm-attr 'name src) src))
      ;; (helm-attrset 'helm-gtags-base-directory dir src)
      (helm-attrset 'helm-gtags-tag-location-list custom-dirs src)
      ;; (print custom-dirs)
      (helm-attrset 'name
                    (format "Searched %s at %s" (or (helm-attr 'init-name src) "")
                            (mapconcat 'identity custom-dirs "  "))
                    src))
    (helm :sources srcs
          :input (or input (thing-at-point 'symbol))
          :buffer buf)))

;;;###autoload
(defun helm-gtags-find-tag-and-symbol ()
  "Jump to definition"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-tags
                       helm-source-gtags-gsyms)))

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
  "Find file with gnu global
you could add `helm-source-gtags-files' to `helm-for-files-preferred-list'"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-files) ""))

;;;###autoload
(defun helm-gtags-find-tag-from-here ()
  "Find from here with gnu global"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-find-tag-from-here)))

(defun helm-gtags-set-parsed-file ()
  (let* ((this-file (file-name-nondirectory (buffer-file-name)))
         (file (if current-prefix-arg
                   (read-file-name "Parsed File: " nil this-file)
                 this-file)))
    (setq helm-gtags-parsed-file (expand-file-name file))))

;;;###autoload
(defun helm-gtags-parse-file ()
  "Find file with gnu global"
  (interactive)
  (helm-gtags-searched-directory)
  (helm-gtags-save-current-context)
  (when (helm-gtags--using-other-window-p)
    (setq helm-gtags-use-otherwin t))
  (helm-gtags-set-parsed-file)
  (helm-attrset 'name
                (format "Parsed File: %s"
                        (file-relative-name helm-gtags-parsed-file
                                            helm-gtags-tag-location))
                helm-source-gtags-parse-file)
  (helm :sources '(helm-source-gtags-parse-file)
        :buffer (get-buffer-create helm-gtags-buffer)))

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
  (setq helm-gtags-context-stack nil))

(defun helm-gtags--update-tags-sentinel (process state)
  (when (eq (process-status process) 'exit)
    (if (zerop (process-exit-status process))
        (message "Update TAGS successfully")
      (message "Failed to update TAGS"))))

(defsubst helm-gtags--update-tags-command (single-update)
  (format "global -u %s" (if single-update
                             ""
                           (format "--single-update=\"%s\"" (buffer-file-name)))))

;;;###autoload
(defun helm-gtags-update-tags ()
  "Update TAG file. Update All files with `C-u' prefix"
  (interactive)
  (when (or (buffer-file-name) current-prefix-arg)
    (let* ((cmd (helm-gtags--update-tags-command current-prefix-arg))
           (proc (start-process-shell-command "helm-gtags-update" nil cmd)))
      (unless proc
        (message "Failed: '%s'" cmd))
      (set-process-sentinel proc 'helm-gtags--update-tags-sentinel))))

(defvar helm-gtags-mode-name " HGtags")
(defvar helm-gtags-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-gtags-mode ()
  "Enable for helm-gtags"
  :group      'helm-gtags
  :init-value nil
  :global     nil
  :keymap     helm-gtags-mode-map
  :lighter    helm-gtags-mode-name
  (if helm-gtags-mode
      (progn
        (run-hooks 'helm-gtags-mode-hook)
        (when helm-gtags-auto-update
          (add-hook 'after-save-hook 'helm-gtags-update-tags nil t)))
    (progn
      (when helm-gtags-auto-update
        (remove-hook 'after-save-hook 'helm-gtags-update-tags t)))))

(provide 'helm-gtags)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags.el ends here
