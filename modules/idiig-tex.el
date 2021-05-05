;; latex functions from spacemacs
(defun latex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))
;; (setq build-proc (TeX-command latex-build-command 'TeX-master-file -1))
;; ;; Sometimes, TeX-command returns nil causing an error in set-process-sentinel
;; (when build-proc
;;   (set-process-sentinel build-proc 'latex//build-sentinel))))

(defun latex//build-sentinel (process event)
  (if (string= event "finished\n")
      (TeX-view)
    (message "Errors! Check with C-`")))

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

;; Rebindings for TeX-font
(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-medium () (interactive) (TeX-font nil ?\C-m))
(defun latex/font-code () (interactive) (TeX-font nil ?\C-t))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
(defun latex/font-italic () (interactive) (TeX-font nil ?\C-i))
(defun latex/font-clear () (interactive) (TeX-font nil ?\C-d))
(defun latex/font-calligraphic () (interactive) (TeX-font nil ?\C-a))
(defun latex/font-small-caps () (interactive) (TeX-font nil ?\C-c))
(defun latex/font-sans-serif () (interactive) (TeX-font nil ?\C-f))
(defun latex/font-normal () (interactive) (TeX-font nil ?\C-n))
(defun latex/font-serif () (interactive) (TeX-font nil ?\C-r))
(defun latex/font-oblique () (interactive) (TeX-font nil ?\C-s))
(defun latex/font-upright () (interactive) (TeX-font nil ?\C-u))

;; auctex
(use-package tex
  :ensure nil
  :defer 3
  :hook (
         (LaTeX-mode-hook . turn-on-reftex)
         (LaTeX-mode-hook . evil-matchit-mode)
         )
  :init
  (progn 
    (setq latex-backend "company-auctex")  ;; 补全
    (setq latex-build-command "latexmk")
    (setq latex-build-engine 'xetex)
    (setq latex-enable-auto-fill t)
    (setq latex-enable-folding t)
    (setq latex-enable-magic t)
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
    (defvar latex-refresh-preview nil
      "Whether non-nil, refresh the preview buffer when file changes.")
    
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-syntactic-comment t
          ;; Synctex support
          TeX-source-correlate-start-server nil
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)
    (when latex-enable-auto-fill
      (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode))
    (when latex-enable-folding
      (add-hook 'LaTeX-mode-hook 'TeX-fold-mode))
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))
  :config
  (progn
    ;; Key bindings for plain TeX
    (dolist (mode '(tex-mode latex-mode context-mode))
      (evil-leader/set-key-for-mode mode
        "\\"  'TeX-insert-macro                            ;; C-c C-m
        "-"   'TeX-recenter-output-buffer                  ;; C-c C-l
        "%"   'TeX-comment-or-uncomment-paragraph          ;; C-c %
        ";"   'comment-or-uncomment-region                 ;; C-c ; or C-c :
        ;; TeX-command-run-all runs compile and open the viewer
        "a"   'TeX-command-run-all                         ;; C-c C-a
        "b"   'latex/build
        "k"   'TeX-kill-job                                ;; C-c C-k
        "l"   'TeX-recenter-output-buffer                  ;; C-c C-l
        "m"   'TeX-insert-macro                            ;; C-c C-m
        "n"   'TeX-next-error                              ;; C-c `
        "N"   'TeX-previous-error                          ;; M-g p
        "v"   'TeX-view                                    ;; C-c C-v
        ;; TeX-doc is a very slow function
        "hd"  'TeX-doc
        "xb"  'latex/font-bold
        "xc"  'latex/font-code
        "xe"  'latex/font-emphasis
        "xi"  'latex/font-italic
        "xr"  'latex/font-clear
        "xo"  'latex/font-oblique
        "xfc" 'latex/font-small-caps
        "xff" 'latex/font-sans-serif
        "xfr" 'latex/font-serif)
      (when idiig-major-mode-emacs-leader-key
        (idiig/set-leader-keys-for-major-mode mode
          idiig-major-mode-emacs-leader-key 'TeX-command-master))
      (when idiig-major-mode-leader-key
        (idiig/set-leader-keys-for-major-mode mode
          idiig-major-mode-leader-key 'TeX-command-master))
      (when latex-enable-folding
        (idiig/set-leader-keys-for-major-mode mode
          ;; the following commands are mostly not autoloaded, but that's fine
          ;; because `TeX-fold-mode' is added to `LaTeX-mode-hook'
          "z=" 'TeX-fold-math
          "zb" 'TeX-fold-buffer
          "zB" 'TeX-fold-clearout-buffer
          "ze" 'TeX-fold-env
          "zI" 'TeX-fold-clearout-item
          "zm" 'TeX-fold-macro
          "zp" 'TeX-fold-paragraph
          "zP" 'TeX-fold-clearout-paragraph
          "zr" 'TeX-fold-region
          "zR" 'TeX-fold-clearout-region
          "zz" 'TeX-fold-dwim))
      (which-key-declare-prefixes-for-mode mode "SPC mh" "help")
      (which-key-declare-prefixes-for-mode mode "SPC mx" "text/fonts")
      (which-key-declare-prefixes-for-mode mode "SPC mz" "fold"))

    ;; Key bindings specific to LaTeX
    (evil-leader/set-key-for-mode 'latex-mode
      "*"   'LaTeX-mark-section      ;; C-c *
      "."   'LaTeX-mark-environment  ;; C-c .
      "c"   'LaTeX-close-environment ;; C-c ]
      "e"   'LaTeX-environment       ;; C-c C-e
      "ii"   'LaTeX-insert-item       ;; C-c C-j
      "s"   'LaTeX-section           ;; C-c C-s 
      "fe"  'LaTeX-fill-environment  ;; C-c C-q C-e
      "fp"  'LaTeX-fill-paragraph    ;; C-c C-q C-p
      "fr"  'LaTeX-fill-region       ;; C-c C-q C-r
      "fs"  'LaTeX-fill-section      ;; C-c C-q C-s
      "pb"  'preview-buffer
      "pc"  'preview-clearout
      "pd"  'preview-document
      "pe"  'preview-environment
      "pf"  'preview-cache-preamble
      "pp"  'preview-at-point
      "pr"  'preview-region
      "ps"  'preview-section
      "xB"  'latex/font-medium
      "xr"  'latex/font-clear
      "xfa" 'latex/font-calligraphic
      "xfn" 'latex/font-normal
      "xfu" 'latex/font-upright
      ;; reftex
      "rc"    'reftex-citation
      "rg"    'reftex-grep-document
      "ri"    'reftex-index-selection-or-word
      "rI"    'reftex-display-index
      "r TAB" 'reftex-index
      "rl"    'reftex-label
      "rp"    'reftex-index-phrase-selection-or-word
      "rP"    'reftex-index-visit-phrases-buffer
      "rr"    'reftex-reference
      "rs"    'reftex-search-document
      "rt"    'reftex-toc
      "rT"    'reftex-toc-recenter
      "rv"    'reftex-view-crossref)
    (which-key-declare-prefixes-for-mode 'latex-mode "SPC mi" "insert")
    (which-key-declare-prefixes-for-mode 'latex-mode "SPC mp" "preview")
    (which-key-declare-prefixes-for-mode 'latex-mode "SPC mf" "fill")
    (which-key-declare-prefixes-for-mode 'latex-mode "SPC mr" "reftex")
    (which-key-declare-prefixes-for-mode 'latex-mode ",i" "insert")
    (which-key-declare-prefixes-for-mode 'latex-mode ",p" "preview")
    (which-key-declare-prefixes-for-mode 'latex-mode ",f" "fill")
    (which-key-declare-prefixes-for-mode 'latex-mode ",r" "reftex")))

;; latexmk
(use-package auctex-latexmk
  :defer t
  :hook (LaTeX-mode-hook . (lambda () (require 'auctex-latexmk)))
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (progn
    (auctex-latexmk-setup)))

;; auto completion
(use-package company-auctex
  :defer t
  :hook (LaTeX-mode-hook . (lambda () (company-auctex-init))))

;; reftex auto completion
(use-package company-reftex
  :defer t
  :hook (LaTeX-mode-hook . (lambda () (company-reftex-init))))

;; magic buffer
(use-package magic-latex-buffer
  :defer t
  :hook (LaTeX-mode-hook . (lambda () (require 'magic-latex-buffer)))
  :init
  (progn
    (add-hook 'TeX-update-style-hook 'magic-latex-buffer)
    (setq magic-latex-enable-block-highlight t
          magic-latex-enable-suscript t
          magic-latex-enable-pretty-symbols t
          magic-latex-enable-block-align nil
          magic-latex-enable-inline-image nil)))

(provide 'idiig-tex)
