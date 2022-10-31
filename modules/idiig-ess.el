;; R程序的位置
(defvar idiig-R-program-path "/usr/local/bin/R")

;; https://github.com/syl20bnr/idiig/blob/master/layers/%2Blang/ess/packages.el
(use-package ess
  :commands (S-mode
             R-mode
             Rnw-mode
             Snw-mode
             omegahat-mode
             XLS-mode
             STA-mode
             SAS-mode
             ess-julia-mode
             S-transcript-mode
             R-transcript-mode
             Rd-mode
             ess-bugs-mode
             ess-jags-mode)
  :defer t
  :mode (("\\.sp\\'"           . S-mode)
         ("/R/.*\\.q\\'"       . R-mode)
         ("\\.[qsS]\\'"        . S-mode)
         ("\\.ssc\\'"          . S-mode)
         ("\\.SSC\\'"          . S-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]nw\\'"       . Rnw-mode)
         ("\\.[sS]nw\\'"       . Snw-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.omg\\'"          . omegahat-mode)
         ("\\.hat\\'"          . omegahat-mode)
         ("\\.lsp\\'"          . XLS-mode)
         ("\\.do\\'"           . STA-mode)
         ("\\.ado\\'"          . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
         ("\\.jl\\'"           . ess-julia-mode)
         ("\\.[Ss]t\\'"        . S-transcript-mode)
         ("\\.Sout"            . S-transcript-mode)
         ("\\.[Rr]out"         . R-transcript-mode)
         ("\\.Rd\\'"           . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  ;; :commands (R stata julia SAS)
  :config
  (progn
    (require 'ess-site)
    (idiig/register-repl 'ess-site 'julia)
    (idiig/register-repl 'ess-site 'R)
    (idiig/register-repl 'ess-site 'SAS)
    (idiig/register-repl 'ess-site 'stata)
    ;; ess mode 本身不会被识别为 prog mode 因此加入
    (add-hook 'ess-mode-hook 'idiig/run-prog-mode-hooks)
    (add-hook 'ess-mode-hook '(lambda () (flymake-mode nil)))))


;; R --------------------------------------------------------------------------
(with-eval-after-load 'ess-site
  ;; Follow Hadley Wickham's R style guide
  (setq inferior-ess-r-program (expand-file-name idiig-R-program-path))
  (setq ess-first-continued-statement-offset 2
        ess-continued-statement-offset 0
        ess-expression-offset 2
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'DEFAULT)

  (defun idiig/ess-start-repl ()
    "Start a REPL corresponding to the ess-language of the current buffer."
    (interactive)
    (cond
     ((string= "S" ess-language) (call-interactively 'R))
     ((string= "STA" ess-language) (call-interactively 'stata))
     ((string= "SAS" ess-language) (call-interactively 'SAS))))

  (evil-leader/set-key-for-mode 'ess-julia-mode
    "'"  'julia
    "si" 'julia)
  
(evil-leader/set-key-for-mode 'ess-mode
    "'"  'idiig/ess-start-repl
    "si" 'idiig/ess-start-repl
    ;; noweb
    "cC" 'ess-eval-chunk-and-go
    "cc" 'ess-eval-chunk
    "cd" 'ess-eval-chunk-and-step
    "cm" 'ess-noweb-mark-chunk
    "cN" 'ess-noweb-previous-chunk
    "cn" 'ess-noweb-next-chunk
    ;; REPL
    "sB" 'ess-eval-buffer-and-go
    "sb" 'ess-eval-buffer
    "sD" 'ess-eval-function-or-paragraph-and-step
    "sd" 'ess-eval-region-or-line-and-step
    "sL" 'ess-eval-line-and-go
    "sl" 'ess-eval-line
    "sR" 'ess-eval-region-and-go
    "sr" 'ess-eval-region
    "sT" 'ess-eval-function-and-go
    "st" 'ess-eval-function
    ;; R helpers
    "hd" 'ess-R-dv-pprint
    "ht" 'ess-R-dv-ctable
    )
  (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
  (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
  (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input))

(use-package ess-smart-equals
  :init (setq ess-smart-equals-extra-ops '(brace paren percent))
  :after (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
  :config (ess-smart-equals-activate))

(provide 'idiig-ess)
