# helm-gtags.el

## Introduction
`helm-gtags.el` is GNU GLOBAL helm interface.

**helm-gtags.el** is not compatible **anything-gtags.el**.
But `helm-gtags.el` is designed for faster search than `anything-gtags.el`.

`anything-gtags.el` is slow in large source tree such as Linux kernel,
FreeBSD, Android etc. Because `anything-gtags.el` creates candidates
by processing output of `gtags.el`. `helm-gtags.el` creates candidates
by itself, so `helm-gtags.el` is faster than `anything-gtags.el`.


## Screenshot

![helm-gtags](image/helm-gtags.png)


## Requirements
* Emacs 23 or higher.
* helm 1.0 or higher
* GNU Global 5.7.1 or higher

`helm-gtags.el` does not require `gtags.el`.


## Basic Usage

#### helm-gtags-mode

Enable `helm-gtags-mode`.

#### helm-gtags-find-tag

Input tag name and move to the definition.

#### helm-gtags-find-rtag

Input tag name and move to the referenced point.

#### helm-gtags-find-symbol

Input symbol and move to the locations.

#### helm-gtags-find-files

Input file name and open it.

If you use `C-u`(prefix argument) before `helm-gtags-find-(tag|rtag|symbol)` functions,
then `helm-gtags.el` lets you input directory and searchs
(definition|reference|symbol) under specified directory.
And `helm-gtags.el` searchs them under current directory with
`C-u C-u` prefix argument

#### helm-gtags-select

Tag jump using gtags and helm(experimental)

#### helm-gtags-pop-stack

Move to previous point on the stack.
helm-gtags pushes current point to stack before executing each jump functions.

#### helm-gtags-show-stack

Show context stack with helm interface.
You can jump to the context.

#### helm-gtags-clear-stack

Clean helm point stack.


## Customize

#### helm-gtags-path-style

File path style, `'root` or `'relative` or `'absolute`(Default is `'root`)

#### helm-gtags-ignore-case

Ignore case for searching flag (Default is `nil`)

#### helm-gtags-read-only

Open file as readonly, if this value is `non-nil`(Default is `nil`).


## Sample Configuration

```elisp
(require 'helm-config)
(require 'helm-gtags)

;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(setq helm-gtags-path-style 'relative)
(setq helm-gtags-ignore-case t)
(setq helm-gtags-read-only t)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))
```
