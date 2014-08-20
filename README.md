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
* Emacs 24 or higher.
* helm 1.5.6 or higher
* GNU Global 6.2.3 or higher

`helm-gtags.el` does not require `gtags.el`.


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

#### `helm-gtags-dwim`

Find name by context.

- Jump to header file if cursor is on include statement
- Jump to tag definition if cursor is on tag reference
- Jump to tag reference if cursor is on tag definition


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

#### `helm-gtags-resume`

Resurrect previously invoked `helm-gtags` command.
This is similar to `helm-resume` however this command resurrects helm gtags
buffer if other helm commands are called.


## Customize Variables

#### `helm-gtags-path-style`(Default `'root`)

File path style, `'root` or `'relative` or `'absolute`

#### `helm-gtags-ignore-case`(Default `nil`)

Ignore case for searching flag

#### `helm-gtags-read-only`(Default `nil`)

Open file as readonly, if this value is `non-nil`

#### `helm-gtags-use-input-at-cursor`(Default `nil`)

Use word at cursor as input if this value is `non-nil`

#### `helm-gtags-highlight-candidate`(Default `t`)

Highlighting candidates if this value is `non-nil`

#### `helm-gtags-auto-update`(Default `nil`)

If this variable is non-nil, TAG file is updated after saving buffer

#### `helm-gtags-update-interval-second`(Default `60`)

Tags are updated in `after-save-hook' if this seconds is passed from last update
Always update if value of this variable is nil.

#### `helm-gtags-cache-select-result`(Default `nil`)

If this variable is non-nil, use cache for `helm-gtags-select` and `helm-gtags-select-path`

#### `helm-gtags-cache-max-result-size`(Default `10MB`)

Max size(bytes) to cache for each select result

#### `helm-gtags-pulse-at-cursor`(Default `nil`)

If this variable is non-nil, pulse at point after jumping

#### `helm-gtags-maximum-candidates`(Default `9999`)

Maximum number of helm candidates in `helm-gtags.el`.
If you feel slow for big source tree such as linux kernel,
please set small number to this variable.


## Faces

#### `helm-gtags-file`

Face of file name of candidates

#### `helm-gtags-lineno`

Face of line number of candidates


## Sample Configuration

```elisp
;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

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
