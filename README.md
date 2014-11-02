# helm-gtags.el [![travis badge][travis-badge]][travis-link] [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

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


## Use Ctags with helm-gtags

You can use `helm-gtags` for languages which are supported by `ctags`
with ctags backend feature of `GNU global`. You can generate `ctags` backend
tags by following command.

```
 % gtags --gtagslabel=ctags
```


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

#### `helm-gtags-tags-in-this-function`

Show tagnames which are referenced in this function and jump to them.


#### `helm-gtags-update-tags`

Update TAG file. Default is update only current file,
You can update all files with `C-u` prefix.

#### `helm-gtags-create-tags`

Create TAG file.

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

## Using Suggested Key Mapping

`helm-gtags.el` provides suggested key maps like `gtags.el` by setting
`helm-gtags-suggested-key-mapping` to non-nil. Its prefix key is `C-c`
as default. You can change prefix by setting `helm-gtags-prefix-key`.

These value must be set before loading `helm-gtags.el`.
I recommend you to use `custom-set-variables` for setting this value.

```lisp
(custom-set-variables
 '(helm-gtags-prefix-key "C-t")
 '(helm-gtags-suggested-key-mapping t))
```

### Default Key Mapping

|Key         |Command                          |
|:-----------|:--------------------------------|
|Prefix `h`  | helm-gtags-display-browser      |
|Prefix `C-]`| helm-gtags-find-tag-from-here   |
|Prefix `C-t`| helm-gtags-pop-stack            |
|Prefix `P`  | helm-gtags-find-files           |
|Prefix `f`  | helm-gtags-parse-file           |
|Prefix `g`  | helm-gtags-find-pattern         |
|Prefix `s`  | helm-gtags-find-symbol          |
|Prefix `r`  | helm-gtags-find-rtag            |
|Prefix `t`  | helm-gtags-find-tag             |
|Prefix `d`  | helm-gtags-find-tag             |
|M-*         | helm-gtags-pop-stack            |
|M-.         | helm-gtags-find-tag             |
|C-x 4 .     | helm-gtags-find-tag-other-window|


## Customize Variables

#### `helm-gtags-path-style`(Default `'root`)

File path style, `'root` or `'relative` or `'absolute`.
You can only use `'absolute` if you use Windows and set `GTAGSLIBPATH` environment variable.
helm-gtags.el forces to use absolute style in such case.

#### `helm-gtags-ignore-case`(Default `nil`)

Ignore case for searching flag

#### `helm-gtags-read-only`(Default `nil`)

Open file as readonly, if this value is `non-nil`

#### `helm-gtags-use-input-at-cursor`(Default `nil`)

Use word at cursor as input if this value is `non-nil`

#### `helm-gtags-highlight-candidate`(Default `t`)

Highlighting candidates if this value is `non-nil`

#### `helm-gtags-display-style`(Default `nil`)

Show detail information if this value is `'detail`,
show reference point of function etc.

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
Please set small number if you feel slow for large source tree
such as Linux kernel.


## Faces

#### `helm-gtags-file`

Face of file name of candidates

#### `helm-gtags-lineno`

Face of line number of candidates


## Sample Configuration

```lisp
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

[travis-badge]: https://travis-ci.org/syohex/emacs-helm-gtags.png
[travis-link]: https://travis-ci.org/syohex/emacs-helm-gtags
[melpa-link]: http://melpa.org/#/helm-gtags
[melpa-stable-link]: http://melpa.org/#/helm-gtags
[melpa-badge]: http://melpa.org/packages/helm-gtags-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/helm-gtags-badge.svg
