;; ~/.emacs.d/init.el
 
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(require 'scala-mode-auto)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
