(use-package ispell
  :defer t
  :init
  ;; ispell不检查部分
  (defun idiig/org-ispell ()
    "Configure `ispell-skip-region-alist' for `org-mode'."
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
  (add-hook 'org-mode-hook #'idiig/org-ispell)
  :config
  (setq ispell-program-name "/opt/homebrew/bin/aspell"))

(use-package lsp-grammarly
  :defer t)

(provide 'idiig-text-checker)
