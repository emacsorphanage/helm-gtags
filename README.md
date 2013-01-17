helm-gtags.el
==================

Introduction
------------
`helm-gtags.el` is GNU GLOBAL helm interface.

**helm-gtags.el** is not compatible **anything-gtags.el**

`anything-gtags.el` is slow in large source tree such as Linux kernel,
FreeBSD, Android etc. Because `anything-gtags.el` creates candidates
by processing output of `gtags.el`. `helm-gtags.el` creates candidates
by itself, so `helm-gtags.el` is faster than `anything-gtags.el`.


Requirements
------------
* Emacs 22.1 or higher.
* helm 1.0 or higher
* GNU Global 5.7.1 or higher

`helm-gtags.el` does not require `gtags.el`.


Basic Usage
-----------

Enable `helm-gtags-mode`.

    M-x helm-gtags-mode

Input tag name and move to the definition.

    M-x helm-gtags-find-tag

Input tag name and move to the referenced point.

    M-x helm-gtags-find-rtag

Input symbol and move to the locations.

    M-x helm-gtags-find-symbol

Input file name and open it.

    M-x helm-gtags-find-files

If you use `C-u`(prefix argument) before `helm-gtags-find-(tag|rtag|symbol)` functions,
then `helm-gtags.el` lets you input directory and searchs
(definition|reference|symbol) under specified directory.

Tag jump using gtags and helm(experimental)

    M-x helm-gtags-select

Move to previous point on the stack.
helm-gtags pushes current point to stack before executing each jump functions.

    M-x helm-gtags-pop-stack

Clean helm point stack.

    M-x helm-gtags-clear-stack


Customize
---------

File path style, root or relative or absolute (Default is root)

    helm-c-gtags-path-style

Ignore case for searching flag (Default is nil)

    helm-c-gtags-ignore-case

Readonly flag, when open file searched.

    helm-c-gtags-read-only(Default is nil)


Sample Configuration
--------------------

```` elisp
(require 'helm-config)
(require 'helm-gtags)

(add-hook 'c-mode-hook (lambda () (helm-gtags-mode)))

;; customize
(setq helm-c-gtags-path-style 'relative)
(setq helm-c-gtags-ignore-case t)
(setq helm-c-gtags-read-only t)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))
````
