helm-gtags.el
==================

Introduction
------------
`helm-gtags.el` is GNU GLOBAL helm interface.

**helm-gtags.el** is not compatible **anything-gtags.el**

*WARNINGS: THIS IS ALPHA VERSION.*


Requirements
------------
* Emacs 22.1 or higher.
* GNU Global 5.7 or higher(`global` and `gtags.el`)


Basic Usage
-----------

Input tag name and move to the definition

    M-x helm-gtags-find-tag

Input tag name and move to the referenced point.

    M-x helm-gtags-find-rtag

Input symbol and move to the locations.

    M-x helm-gtags-find-symbol

Input symbol and move to the locations.

    M-x helm-gtags-find-files


Sample Configuration
--------------------
You add

    (require 'helm-config)
    (require 'helm-gtags)

    (add-hook 'c-mode-hook (lambda () (gtags-mode)))

    ;; path-style is 'root, 'relative or 'absolute. Default is 'root
    (setq helm-c-gtags-path-style 'relative)

    (setq gtags-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
             (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
             (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
             (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))
