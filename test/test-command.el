;;; test-command.el --- test commands of helm-gtags

;; Copyright (C) 2014 by Syohei YOSHIDA

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

(ert-deftest helm-gtags-clear-all-cache ()
  "Clear all caches"
  (let ((helm-gtags--result-cache (make-hash-table :test 'equal)))
    (puthash "foo" 'foo helm-gtags--result-cache)
    (puthash "bar" 'bar helm-gtags--result-cache)
    (call-interactively 'helm-gtags-clear-all-cache)
    (should (= (hash-table-count helm-gtags--result-cache) 0))))

(ert-deftest helm-gtags-clear-cache ()
  "Clear caches"
  (cl-letf (((symbol-function 'helm-gtags--find-tag-directory)
             'ignore))
    (let ((helm-gtags--result-cache (make-hash-table :test 'equal))
          (helm-gtags--real-tag-location "foo/"))
      (puthash "foo/GTAGS" 'foo1 helm-gtags--result-cache)
      (puthash "foo/GPATH" 'foo2 helm-gtags--result-cache)
      (puthash "bar" 'bar helm-gtags--result-cache)
      (call-interactively 'helm-gtags-clear-cache)
      (should (= (hash-table-count helm-gtags--result-cache) 1))
      (should (eq (gethash "bar" helm-gtags--result-cache) 'bar)))))

(ert-deftest helm-gtags-clear-stack ()
  "Clear current stack"
  (cl-letf (((symbol-function 'helm-gtags--find-tag-directory)
             (lambda () "foo")))
    (let ((helm-gtags--context-stack (make-hash-table :test 'equal)))
      (puthash "foo" 'foo helm-gtags--context-stack)
      (puthash "bar" 'bar helm-gtags--context-stack)
      (call-interactively 'helm-gtags-clear-stack)
      (should (eq (gethash "foo"  helm-gtags--context-stack 'deleted) 'deleted))
      (should (= (hash-table-count helm-gtags--context-stack) 1)))))

(ert-deftest helm-gtags-clear-all-stacks ()
  "Clear current stack"
  (let ((helm-gtags--context-stack (make-hash-table :test 'equal)))
    (puthash "foo" 'foo helm-gtags--context-stack)
    (puthash "bar" 'bar helm-gtags--context-stack)
    (call-interactively 'helm-gtags-clear-all-stacks)
    (should (= (hash-table-count helm-gtags--context-stack) 0))))

;;; test-command.el ends here
