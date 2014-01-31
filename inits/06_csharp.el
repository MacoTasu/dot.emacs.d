(require 'csharp-mode)
(add-hook 'csharp-mode-hook (lambda ()
                              (setq c-basic-offset 4
                                    tab-width 4
                                    indent-tabs-mode t
                                    text-mode-hook 'turn-off-auto-fill
                                    buffer-file-coding-system utf-8-with-signature-dos
                                    )))

(add-hook 'java-mode-hook (lambda () (setq tab-width 4)))
(autoload 'csharp-mode "csharp-mode" "C# editing mode." t)
(setq auto-mode-alist
    (append '(
        ("\\.s?html?\\'" . html-helper-mode)
        ("\\.asp$" . html-helper-mode)
        ("\\.as[phm]x$" . html-helper-mode)
        ("\\.html$" . html-helper-mode)
        ("\\.htm$" . html-helper-mode)
                    ("\\.md$" . emacs-lisp-mode)
        ("\\.txt$" . text-mode)
        ("\\.cs$" . csharp-mode)
        ("\\.faced$" . csharp-mode)
    ) auto-mode-alist ))
