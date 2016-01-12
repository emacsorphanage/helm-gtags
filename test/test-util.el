;;; test-util.el --- Test utilities of helm-gtags

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'ert)
(require 'helm-gtags)

(defmacro with-gtags-project (dirname &rest body)
  "Create temporary gtags project, executes body, remove temporary project."
  (declare (indent 1) (debug t))
  (let ((orig-dir (cl-gensym)))
    `(let ((,orig-dir default-directory))
       (make-directory ,dirname)
       (with-temp-file (concat ,dirname "test.c")
         (insert "int func(void) {}\n"))
       (let ((default-directory ,dirname))
         (unwind-protect
             (progn
               (process-file "gtags")
               ,@body)
           (let ((default-directory ,orig-dir))
             (delete-directory ,dirname t nil)))))))

(defsubst dummy-directory ()
  (file-name-as-directory (concat (expand-file-name default-directory) "dummy")))

(ert-deftest helm-gtags--set-parsed-file ()
  "Test utility `helm-gtags--set-parsed-file'"
  (let ((this-file (expand-file-name "./this-util.el")))
    (with-current-buffer (find-file-noselect this-file)
      (should (string= (helm-gtags--set-parsed-file) this-file))
      (should helm-gtags--parsed-file)
      (should (string= helm-gtags--parsed-file this-file)))))

(ert-deftest helm-gtags--path-libpath-p ()
  "Test utility `helm-gtags--path-libpath-p'"
  (let ((process-environment '("GTAGSLIBPATH=/foo:/bar:/baz")))
    (should (helm-gtags--path-libpath-p "/foo/"))))

(ert-deftest helm-gtags--tag-directory ()
  "Test utility `helm-gtags--tag-directory'"
  (let ((dummy (dummy-directory)))
    (with-gtags-project dummy
      (should (string= (helm-gtags--tag-directory) dummy)))))

(ert-deftest helm-gtags--find-tag-directory ()
  "Test utility `helm-gtags--find-tag-directory'"
  (let ((dummy (dummy-directory)))
    (with-gtags-project (dummy-directory)
      (let ((got (helm-gtags--find-tag-directory)))
        (should (string= (helm-gtags--find-tag-directory) dummy))
        (should (string= helm-gtags--tag-location dummy))))))

(ert-deftest helm-gtags--find-tag-directory-in-libpath ()
  "Test utility `helm-gtags--find-tag-directory' in library path"
  (let ((dummy (dummy-directory)))
    (with-gtags-project (dummy-directory)
      (let* ((process-environment (list (concat "GTAGSLIBPATH=" dummy)))
             (helm-gtags--tag-location "/tmp/")
             (got (helm-gtags--find-tag-directory)))
        (should (string= (helm-gtags--find-tag-directory) "/tmp/"))
        (should (string= helm-gtags--real-tag-location dummy))))))

(ert-deftest helm-gtags--construct-options ()
  "Test utility `helm-gtags--construct-options'"
  (let ((got (helm-gtags--construct-options 'find-file t)))
    (should (equal got '("-Poa" "-c"))))

  (let ((got (helm-gtags--construct-options 'tag nil)))
    (should (equal got '("--result=grep"))))

  (let* ((helm-gtags-path-style 'absolute)
         (helm-gtags-ignore-case t)
         (current-prefix-arg t)
         (process-environment (list "GTAGSLIBPATH=foo:bar" ))
         (got (helm-gtags--construct-options 'symbol t)))
    (should (equal got '("-T" "-l" "-i" "-a" "-s" "-c" "--result=grep")))))

(ert-deftest helm-gtags--construct-options-force-abs-option ()
  "Test utility `helm-gtags--construct-options' for special case of Windows system"

  (let* ((system-type 'windows-nt)
         (process-environment (list "GTAGSLIBPATH=foo" ))
         (helm-gtags-path-style 'relative)
         (got (helm-gtags--construct-options 'tag t)))
    (should (member "-a" got)))

  (let* ((system-type 'gnu/linux)
         (process-environment (list "GTAGSLIBPATH=foo" ))
         (helm-gtags-path-style 'root)
         (got (helm-gtags--construct-options 'tag t)))
    (should-not (member "-a" got))))

(ert-deftest helm-gtags--check-browser-installed ()
  "Test utility `helm-gtags--browser-installed-p'"
  (should (ignore-errors (helm-gtags--check-browser-installed "emacs") t))
  (should-error (helm-gtags--check-browser-installed "InternetChromeFox")))

(ert-deftest helm-gtags--how-to-update-tags ()
  "Test utility `helm-gtags--how-to-update-tags'"
  (should (eq (helm-gtags--how-to-update-tags) 'single-update))
  (should (eq (helm-gtags--how-to-update-tags) 'single-update))
  (let ((current-prefix-arg '(4)))
    (should (eq (helm-gtags--how-to-update-tags) 'entire-update)))
  (let ((current-prefix-arg 16))
    (should (eq (helm-gtags--how-to-update-tags) 'generate-other-directory))))

(ert-deftest helm-gtags--extract-file-and-line ()
  "Test utility `helm-gtags--extract-file-and-line'"
  (let ((input "C:/Program Files/Microsoft SDKs/Windows/v7.1/Include/Fci.h:44:typedef unsigned int UINT; /* ui */"))
    (let* ((system-type 'windows-nt)
           (got (helm-gtags--extract-file-and-line input))
           (file (car got))
           (line (cdr got)))
      (should (string= file "C:/Program Files/Microsoft SDKs/Windows/v7.1/Include/Fci.h"))
      (should (= line 44)))

    ;; https://github.com/syohex/emacs-helm-gtags/issues/80
    (let* ((system-type 'windows-nt)
           (got (helm-gtags--extract-file-and-line "../include/stdio.h:30:#define hoge 1")))
      (should got)
      (let ((file (car got))
            (line (cdr got)))
       (should (string= file "../include/stdio.h"))
       (should (= line 30))))

    (let* ((system-type 'gnu/linux)
           (got (helm-gtags--extract-file-and-line "/usr/include/stdio.h:30:#define hoge 1"))
           (file (car got))
           (line (cdr got)))
      (should (string= file "/usr/include/stdio.h"))
      (should (= line 30)))))

(ert-deftest helm-gtags--transformer-regexp ()
  "Test utility `helm-gtags--transformer-regexp'"
  (let ((input "C:/Program Files/Microsoft SDKs/Windows/v7.1/Include/Fci.h:44:typedef unsigned int UINT; /* ui */"))
    (let* ((system-type 'windows-nt)
           (regexp (helm-gtags--transformer-regexp input)))
      (should (string-match regexp input))
      (should (string= (match-string-no-properties 1 input)
                       "C:/Program Files/Microsoft SDKs/Windows/v7.1/Include/Fci.h"))
      (should (string= (match-string-no-properties 2 input) "44")))

    (let* ((system-type 'gnu/linux)
           (input "/usr/include/stdio.h:30:#define hoge 1")
           (regexp (helm-gtags--transformer-regexp input)))
      (should (string-match regexp input))
      (should (string= (match-string-no-properties 1 input) "/usr/include/stdio.h"))
      (should (string= (match-string-no-properties 2 input) "30")))))

(ert-deftest helm-gtags--label-option ()
  "Test utility `helm-gtags--label-option'"
  (let ((option (helm-gtags--label-option "ctags")))
    (should (string= option "--gtagslabel=ctags"))))
