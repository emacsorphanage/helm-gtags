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

#### `helm-gtags-mode`

Enable `helm-gtags-mode`.

#### `helm-gtags-find-tag`

Input tag name and move to the definition.

#### `helm-gtags-find-tag-from-here`

Find tag from here and move to its definition (gnu global --from-here).

#### `helm-gtags-find-rtag`

Input tag name and move to the referenced point.

#### `helm-gtags-find-symbol`

Input symbol and move to the locations.

#### `helm-gtags-find-tag-and-symbol`

 `helm-gtags-find-tag` and  `helm-gtags-find-symbol`
 
#### `helm-gtags-find-files`

Input file name and open it.

You can use those searching commands with prefix key,
though I think this is unless now after support helm-gtags-tag-location-list.

| Prefix Key  | Description                       |
|:------------|:---------------------------------:|
| C-u         | Searches from specified directory |
| C-u C-u     | Searches under current directory  |
| C--         | Jump to symbol with other window  |



#### `helm-gtags-select`

Tag jump using gtags and helm

First list all symbol to select, after you select the symbol
use it as keyword calling one of
  `helm-gtags-find-symbol`
  `helm-gtags-find-rtag`
  `helm-gtags-find-tag`

### `helm-gtags-complete`

Complete symbol at point(help writing code).

#### `helm-gtags-update-tags`

Update TAG file. Default is update only current file.
You can update all files with `C-u` prefix.
You can generate new tags with `C-uC-u` prefix.

#### `helm-gtags-parse-file`

Show symbols in current file like `gtags-parse-file`. You can choose
any files with `C-u` prefix.

#### `helm-gtags-pop-stack`

Move to previous point on the stack.
helm-gtags pushes current point to stack before executing each jump functions.

#### `helm-gtags-show-stack`

Show context stack with helm interface.
You can jump to the context.

#### `helm-gtags-clear-stack`

Clear current context stack.


## Customize Variables

### `helm-gtags-tag-location-list`
You could add your lib directory here ,like
```
(add-to-list 'helm-gtags-tag-location-list "/usr/include/")
```
of cource  you should run :
```
gtags
```
in these directories.
then 
  `helm-gtags-find-symbol`
  `helm-gtags-find-rtag`
  `helm-gtags-find-tag`
  `helm-gtags-find-tag-from-here`
  `helm-gtags-find-files`
  `helm-gtags-select`
could find tag rtags symbol files in several directory with one command
#### `helm-gtags-path-style`

File path style, `'root` or `'relative` or `'absolute`(Default is `'root`)

#### `helm-gtags-ignore-case`

Ignore case for searching flag (Default is `nil`)

#### `helm-gtags-read-only`

Open file as readonly, if this value is `non-nil`(Default is `nil`).


#### `helm-gtags-auto-update`

If this variable is non-nil, TAG file is updated after saving buffer.

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
(setq helm-gtags-auto-update t)

(eval-after-load "helm-gtags" '(add-to-list 'helm-for-files-preferred-list helm-source-gtags-files t))

(add-hook 'c-mode-hook '(lambda()
                          ;;'helm-gtags-tag-location-list is a buffer local var
                          (add-to-list 'helm-gtags-tag-location-list "/usr/include/")
                          ))

;; key bindings
    (add-hook 'helm-gtags-mode-hook
              '(lambda ()
                 (local-set-key [(meta return)] 'helm-gtags-complete)
                 (local-set-key (kbd "M-.") 'helm-gtags-find-tag-and-symbol)
                 (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
                 (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
                 (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
                 (local-set-key (kbd "C-,") 'helm-gtags-pop-stack)
                 (local-set-key (kbd "M-*") 'helm-gtags-show-stack)
                 (local-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)
                 (local-set-key (kbd "C-c C-f") 'helm-gtags-find-files)))
```
