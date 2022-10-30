(use-package yaml-mode
  :bind (:map yaml-mode-map
              ("C-m" . newline-and-indent))
  :config
  (add-hook 'yaml-mode-hook 'idiig/run-prog-mode-hooks))

(provide 'idiig-yaml)
