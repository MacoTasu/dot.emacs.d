(require 'go-mode-load)
(add-hook 'go-mode-hook 'flycheck-mode
          '(lambda()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode t)
            (setq-default tab-width 4)
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c i") 'go-goto-imports)
            (local-set-key (kbd "C-c d") 'godoc)))

(add-hook 'before-save-hook 'gofmt-before-save)
