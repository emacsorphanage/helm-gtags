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
I recommend you to use GNU Global 5.9 or higher version. Because lower
versions are too slow. Debian/Ubuntu GNU global package is too old(5.7.1),
so you should install it from source code.


## Basic Usage

#### `helm-gtags-mode`

Enable `helm-gtags-mode`.

#### `helm-gtags-find-tag`

Input tag name and move to the definition.

#### `helm-gtags-find-tag-from-here`

Find tag from here and move to its definition.

#### `helm-gtags-find-rtag`

Input tag name and move to the referenced point.

#### `helm-gtags-find-symbol`

Input symbol and move to the locations.

#### `helm-gtags-find-files`

Input file name and open it.


You can use those searching commands with prefix key.

| Prefix Key  | Description                       |
|:------------|:---------------------------------:|
| C-u         | Searches from specified directory |
| C-u C-u     | Searches under current directory  |
| C--         | Jump to symbol with other window  |


#### `helm-gtags-select`

Tag jump using gtags and helm


#### `helm-gtags-update-tags`

Update TAG file. Default is update only current file,
You can update all files with `C-u` prefix.

#### `helm-gtags-parse-file`

Show symbols in current file like `gtags-parse-file`. You can choose
any files with `C-u` prefix.

#### `helm-gtags-pop-stack`

Move to previous point on the stack.
helm-gtags pushes current point to stack before executing each jump functions.

#### `helm-gtags-next-history`

Move to next history on context stack.

#### `helm-gtags-previous-history`

Move to previous history on context stack.

#### `helm-gtags-show-stack`

Show context stack with helm interface.
You can jump to the context.

#### `helm-gtags-clear-stack`

Clear current context stack.

#### `helm-gtags-clear-all-stacks`

Clear all context stacks.

#### `helm-gtags-clear-cache`

Clear current project cache for `helm-gtags-select` and `helm-gtags-select-path`

#### `helm-gtags-clear-all-cache`

Clear all result cache for `helm-gtags-select` and `helm-gtags-select-path`


## Customize Variables

#### `helm-gtags-path-style`(Default `'root`)

File path style, `'root` or `'relative` or `'absolute`

#### `helm-gtags-ignore-case`

Ignore case for searching flag (Default `nil`)

#### `helm-gtags-read-only`

Open file as readonly, if this value is `non-nil`(Default `nil`).

#### `helm-gtags-auto-update`

If this variable is non-nil, TAG file is updated after saving buffer.

#### `helm-gtags-update-interval-second`(Default `60`)

Tags are updated in `after-save-hook' if this seconds is passed from last update.
Always update if value of this variable is nil.

#### `helm-gtags-cache-select-result`(Default `nil`)

If this variable is non-nil, use cache for `helm-gtags-select` and `helm-gtags-select-path`

#### `helm-gtags-cache-max-result-size`(Default `10MB`)

Max size(bytes) to cache for each select result


## Sample Configuration

```elisp
;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(setq helm-gtags-path-style 'relative)
(setq helm-gtags-ignore-case t)
(setq helm-gtags-read-only t)
(setq helm-gtags-auto-update t)

;; key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))
```
