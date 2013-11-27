;; ~/.emacs.d/site以下全部読み込み
(let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;;; init-loader
(require 'init-loader)
;;; 設定ファイルのあるフォルダを指定
(setq inits_dir (expand-file-name "~/.emacs.d/inits/"))
(init-loader-load inits_dir)

(require 'package)

; Add package-archives
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")) ; ついでにmarmaladeも追加

; Initialize
(package-initialize)

; melpa.el
(require 'melpa)
