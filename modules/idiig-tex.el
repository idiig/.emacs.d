(defun latex//autofill ()
  "Check whether the pointer is currently inside one of the
environments described in `latex-nofill-env' and if so, inhibits
the automatic filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment latex-nofill-env))))
    (when do-auto-fill
      (do-auto-fill))))

(defun latex/auto-fill-mode ()
  "Toggle auto-fill-mode using the custom auto-fill function."
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'latex//autofill))

;; auctex
(use-package latex
  :ensure auctex
  :hook (
         (LaTeX-mode-hook . turn-on-reftex)
         (LaTeX-mode-hook . evil-matchit-mode)
         )
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (progn 
    ;; (setq latex-backend "company-auctex")  ;; 补全
    (setq latex-build-command "latexmk")
    (setq latex-build-engine 'xetex)
    (setq latex-nofill-env
          '("equation"
            "equation*"
            "align"
            "align*"
            "tabular"
            "tabular*"
            "tabu"
            "tabu*"
            "tikzpicture"))
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-syntactic-comment t
          ;; Synctex support
          TeX-source-correlate-start-server nil
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)
    (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
    (add-hook 'LaTeX-mode-hook 'idiig/run-prog-mode-hooks)
    (add-hook 'TeX-mode-hook 'idiig/run-prog-mode-hooks)))

;; latexmk
(use-package auctex-latexmk
  :defer t
  :hook (LaTeX-mode-hook . (lambda () (require 'auctex-latexmk)))
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (progn
    (auctex-latexmk-setup)))

(provide 'idiig-tex)
