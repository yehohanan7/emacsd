
;; This sets up the load path so that we can override it
(package-initialize nil)
;; Override the packages with the git version of Org and other packages
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)
(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/john.org"))

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(put 'upcase-region 'disabled nil)
