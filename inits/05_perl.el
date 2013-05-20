(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.psgi$" . cperl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.cgi$" . cperl-mode)) auto-mode-alist))
;(setq auto-mode-alist (append '(("\\.pl$" . cperl-mode)) auto-mode-alist))
;(setq auto-mode-alist (append '(("\\.pm$" . cperl-mode)) auto-mode-alist))
(setq make-backup-files nil)
(setq auto-save-default nil)
(add-hook 'cperl-mode-hook
          '(lambda ()
                  (cperl-set-style "PerlStyle"))) 


;;indent
(setq-default indent-tabs-mode nil) 
(custom-set-variables
 '(cperl-indent-level 4)
 '(cperl-continued-statement-offset 4)
 '(cperl-brace-offset -4)
 '(cperl-label-offset -4)
 '(cperl-indent-parens-as-block t)
 '(cperl-close-paren-offset -4)
 '(cperl-tab-always-indent t)
 '(cperl-highlight-variables-indiscriminately t))


;;whitespace and tab
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))

;; ;;補完入れると重い
;; ;;perl-completion
;; (defvar ac-source-my-perl-completion
;;   '((candidates . plcmp-ac-make-cands)))
;; (defun my-cperl-mode-hook ()
;;   (interactive)
;;   (perl-completion-mode t)
;;   (require 'auto-complete)
;;   (add-to-list 'ac-sources 'ac-source-my-perl-completion))
;; (add-hook 'cperl-mode-hook 'my-cperl-mode-hook)
