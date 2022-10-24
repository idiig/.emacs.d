;; preface
(defvar lsp-remap-xref-keybindings nil "When non-nil, xref keybindings remapped to lsp-ui-peek-find-*")
(defvar lsp-navigation 'both
  "If `simple' binds lightweight navigation functions under `SPC m g'.
If `peek' binds lsp-ui navigation functions under `SPC m g'.
If `both', binds lightweight navigation functions under `SPC m g' and lsp-ui functions under `SPC m G'")

;; LSP func
(defun idiig/lsp-ui-doc-func ()
  "Toggle the function signature in the lsp-ui-doc overlay"
  (interactive)
  (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature)))

(defun idiig/lsp-ui-sideline-symb ()
  "Toggle the symbol in the lsp-ui-sideline overlay.
(generally redundant in C modes)"
  (interactive)
  (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol)))

(defun idiig/lsp-ui-sideline-ignore-duplicate ()
  "Toggle ignore duplicates for lsp-ui-sideline overlay"
  (interactive)
  (setq lsp-ui-sideline-ignore-duplicate
        (not lsp-ui-sideline-ignore-duplicate)))

(defun idiig//lsp-action-placeholder ()
  (interactive)
  (message "Not supported yet... (to be implemented in 'lsp-mode')"))

(defun idiig/lsp-define-key (keymap key def &rest bindings)
  "Define multiple key bindings with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun idiig/lsp-bind-keys ()
  "Define key bindings for the lsp minor mode."
  (cl-ecase lsp-navigation
    ('simple (idiig//lsp-bind-simple-navigation-functions "g"))
    ('peek (idiig//lsp-bind-peek-navigation-functions "g"))
    ('both
     (idiig//lsp-bind-simple-navigation-functions "g")
     (idiig//lsp-bind-peek-navigation-functions "G")))

  (idiig/set-leader-keys-for-minor-mode 'lsp-mode
    ;; format
    "=b" #'lsp-format-buffer
    "=r" #'lsp-format-region
    "=o" #'lsp-organize-imports
    ;; code actions
    "aa" #'lsp-execute-code-action
    "af" #'idiig//lsp-action-placeholder
    "ar" #'idiig//lsp-action-placeholder
    "as" #'idiig//lsp-action-placeholder
    ;; goto
    ;; N.B. implementation and references covered by xref bindings / lsp provider...
    "gt" #'lsp-find-type-definition
    "gk" #'idiig/lsp-avy-goto-word
    "gK" #'idiig/lsp-avy-goto-symbol
    "gM" #'lsp-ui-imenu
    ;; help
    "hh" #'lsp-describe-thing-at-point
    ;; jump
    ;; backend
    "bd" #'lsp-describe-session
    "br" #'lsp-workspace-restart
    "bs" #'lsp-workspace-shutdown
    ;; refactor
    "rr" #'lsp-rename
    ;; toggles
    "Td" #'lsp-ui-doc-mode
    "Ts" #'lsp-ui-sideline-mode
    "TF" #'idiig/lsp-ui-doc-func
    "TS" #'idiig/lsp-ui-sideline-symb
    "TI" #'idiig/lsp-ui-sideline-ignore-duplicate
    "Tl" #'lsp-lens-mode
    ;; folders
    "Fs" #'lsp-workspace-folders-switch
    "Fr" #'lsp-workspace-folders-remove
    "Fa" #'lsp-workspace-folders-add
    ;; text/code
    "xh" #'lsp-document-highlight
    "xl" #'lsp-lens-show
    "xL" #'lsp-lens-hide))

;; xref
(defun idiig//lsp-bind-simple-navigation-functions (prefix-char)
  (idiig/set-leader-keys-for-minor-mode 'lsp-mode
    (concat prefix-char "i") #'lsp-find-implementation
    (concat prefix-char "d") #'xref-find-definitions
    (concat prefix-char "r") #'xref-find-references
    (concat prefix-char "e") #'lsp-treemacs-errors-list
    (concat prefix-char "b") #'xref-pop-marker-stack))

;; peek keybindings
(defun idiig//lsp-bind-peek-navigation-functions (prefix-char)
  (idiig/set-leader-keys-for-minor-mode 'lsp-mode
    (concat prefix-char "i") #'lsp-ui-peek-find-implementation
    (concat prefix-char "d") #'lsp-ui-peek-find-definitions
    (concat prefix-char "r") #'lsp-ui-peek-find-references
    (concat prefix-char "s") #'lsp-ui-peek-find-workspace-symbol
    (concat prefix-char "S") #'lsp-treemacs-symbols
    (concat prefix-char "b") #'lsp-ui-peek-jump-backward
    ;; (concat prefix-char "e") #'lsp-ui-flycheck-list
    (concat prefix-char "n") #'lsp-ui-peek-jump-forward))

;; for prefix
(defun idiig//lsp-declare-prefixes-for-mode ()
  "Define key binding prefixes for the specific MODE."
  (which-key-declare-prefixes ",=" "format")
  (which-key-declare-prefixes ",a" "code actions")
  (which-key-declare-prefixes ",b" "backend")
  (which-key-declare-prefixes ",F" "folder")
  (which-key-declare-prefixes ",g" "goto")
  (which-key-declare-prefixes ",G" "peek")
  (which-key-declare-prefixes ",h" "help")
  (which-key-declare-prefixes ",r" "refactor")
  (which-key-declare-prefixes ",T" "toggle")
  (which-key-declare-prefixes ",x" "text/code")
  (dolist (prefix '(",g" ",G"))
    (which-key-declare-prefixes (concat prefix "h") "hierarchy")
    (which-key-declare-prefixes (concat prefix "m") "members"))
  (which-key-declare-prefixes "SPC m=" "format")
  (which-key-declare-prefixes "SPC ma" "code actions")
  (which-key-declare-prefixes "SPC mb" "backend")
  (which-key-declare-prefixes "SPC mF" "folder")
  (which-key-declare-prefixes "SPC mg" "goto")
  (which-key-declare-prefixes "SPC mG" "peek")
  (which-key-declare-prefixes "SPC mh" "help")
  (which-key-declare-prefixes "SPC mr" "refactor")
  (which-key-declare-prefixes "SPC mT" "toggle")
  (which-key-declare-prefixes "SPC mx" "text/code")
  (dolist (prefix '("SPC mg" "SPC mG"))
    (which-key-declare-prefixes (concat prefix "h") "hierarchy")
    (which-key-declare-prefixes (concat prefix "m") "members"))
  (which-key-declare-prefixes "C-SPC m=" "format")
  (which-key-declare-prefixes "C-SPC ma" "code actions")
  (which-key-declare-prefixes "C-SPC mb" "backend")
  (which-key-declare-prefixes "C-SPC mF" "folder")
  (which-key-declare-prefixes "C-SPC mg" "goto")
  (which-key-declare-prefixes "C-SPC mG" "peek")
  (which-key-declare-prefixes "C-SPC mh" "help")
  (which-key-declare-prefixes "C-SPC mr" "refactor")
  (which-key-declare-prefixes "C-SPC mT" "toggle")
  (which-key-declare-prefixes "C-SPC mx" "text/code")
  (dolist (prefix '("C-SPC mg" "SPC mG"))
    (which-key-declare-prefixes (concat prefix "h") "hierarchy")
    (which-key-declare-prefixes (concat prefix "m") "members")))

;; LSP
(use-package lsp-mode
  :defer 3
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil) ;; 去掉header line 
  :hook (
         (prog-mode . (lambda ()
                        ;; hook任意语言
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp-deferred))))
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)))
         (lsp-mode . (lambda () (interactive)
                       ;; keybindings
                       (idiig//lsp-declare-prefixes-for-mode)
                       (idiig/lsp-bind-keys)))
         )
  :commands lsp
  :config
  (progn    
    ;; ui弹窗设置
    (use-package lsp-ui
      :commands lsp-ui-mode
      :custom
      ;; lsp-ui-doc
      (lsp-ui-doc-enable t)
      (lsp-ui-doc-header t)
      (lsp-ui-doc-include-signature t)
      (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
      (lsp-ui-doc-max-width 150)
      (lsp-ui-doc-max-height 30)
      (lsp-ui-doc-use-childframe t)
      (lsp-ui-doc-use-webkit t)
      (lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
      ;; lsp-ui-flycheck
      (lsp-ui-flycheck-enable nil)
      ;; lsp-ui-sideline
      (lsp-ui-sideline-enable nil)
      (lsp-ui-sideline-ignore-duplicate t)
      (lsp-ui-sideline-show-symbol nil)
      (lsp-ui-sideline-show-hover t)
      (lsp-ui-sideline-show-diagnostics nil)
      (lsp-ui-sideline-show-code-actions nil)
      ;; lsp-ui-imenu
      (lsp-ui-imenu-enable nil)
      (lsp-ui-imenu-kind-position 'top)
      ;; lsp-ui-peek
      (lsp-ui-peek-enable t)
      (lsp-ui-peek-peek-height 20)
      (lsp-ui-peek-list-width 50)
      (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
      ;; lsp-ui-imenu
      (lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                             ,(face-foreground 'font-lock-string-face)
                             ,(face-foreground 'font-lock-constant-face)
                             ,(face-foreground 'font-lock-variable-name-face)))
      :preface
      (defun ladicle/toggle-lsp-ui-doc ()
        (interactive)
        (if lsp-ui-doc-mode
            (progn
              (lsp-ui-doc-mode -1)
              (lsp-ui-doc--hide-frame))
          (lsp-ui-doc-mode 1)))
      :bind
      (:map lsp-mode-map
            ("C-c C-r" . lsp-ui-peek-find-references)
            ("C-c j"   . lsp-ui-peek-find-definitions)
            ("C-c i"   . lsp-ui-peek-find-implementation)
            ("C-c m"   . lsp-ui-imenu)
            ("C-c s"   . lsp-ui-sideline-mode)
            ("C-c d"   . ladicle/toggle-lsp-ui-doc))
      :config
      ;; `C-g 关闭文档提示
      (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
      ;; lsp-ui keybindings
      (if lsp-remap-xref-keybindings
          (progn (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
                 (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))
      (idiig/lsp-define-key
       lsp-ui-peek-mode-map
       "h" #'lsp-ui-peek--select-prev-file
       "j" #'lsp-ui-peek--select-next
       "k" #'lsp-ui-peek--select-prev
       "l" #'lsp-ui-peek--select-next-file
       )
      ;; 加载注题后重制 `lsp-ui-doc-background'
      (add-hook 'after-load-theme-hook
                (lambda ()
                  (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
                  (set-face-background 'lsp-ui-doc-background (face-background 'tooltip))))
      )

    ;; ;; lsp-ivy for icons
    ;; (use-package lsp-ivy
    ;;   :commands lsp-ivy-workspace-symbol
    ;;   )
    
    ;; lsp-treemacs
    (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

    ;; optionally if you want to use debugger
    (use-package dap-mode)
    ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

    ;; flymake: error check
    (use-package flymake-posframe
      ;; :ensure nil
      :load-path "~/.emacs.d/dependencies/"
      ;; :after posframe
      :custom
      (flymake-posframe-error-prefix "!! ")
      :hook (flymake-mode . flymake-posframe-mode))

    ;; (use-package flymake-diagnostic-at-point
    ;;   ;; :disabled
    ;;   ;; :after flymake
    ;;   :custom
    ;;   (flymake-diagnostic-at-point-timer-delay 0.1)
    ;;   (flymake-diagnostic-at-point-error-prefix "!! ")
    ;;   (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer) ;; or flymake-diagnostic-at-point-display-popup
    ;;   :hook
    ;;   (flymake-mode . flymake-diagnostic-at-point-mode))
    )
  )


;; ivy integration
;; (defun idiig//lsp-avy-document-symbol (all)
;;   (interactive)
;;   (let ((line 0) (col 0) (w (selected-window))
;;         (ccls (and (memq major-mode '(c-mode c++-mode objc-mode)) (eq c-c++-backend 'lsp-ccls)))
;;         (start-line (1- (line-number-at-pos (window-start))))
;;         (end-line (1- (line-number-at-pos (window-end))))
;;         ranges point0 point1
;;         candidates)
;;     (save-excursion
;;       (goto-char 1)
;;       (cl-loop for loc in
;;                (lsp--send-request
;;                 (lsp--make-request
;;                  "textDocument/documentSymbol"
;;                  `(:textDocument ,(lsp--text-document-identifier)
;;                                  :all ,(if all t :json-false)
;;                                  :startLine ,start-line :endLine ,end-line)))
;;                for range = (if ccls
;;                                loc
;;                              (->> loc (gethash "location") (gethash "range")))
;;                for range_start = (gethash "start" range)
;;                for range_end = (gethash "end" range)
;;                for l0 = (gethash "line" range_start)
;;                for c0 = (gethash "character" range_start)
;;                for l1 = (gethash "line" range_end)
;;                for c1 = (gethash "character" range_end)
;;                while (<= l0 end-line)
;;                when (>= l0 start-line)
;;                do
;;                (forward-line (- l0 line))
;;                (forward-char c0)
;;                (setq point0 (point))
;;                (forward-line (- l1 l0))
;;                (forward-char c1)
;;                (setq point1 (point))
;;                (setq line l1 col c1)
;;                (push `((,point0 . ,point1) . ,w) candidates)))
;;     (avy-with avy-document-symbol
;;               (avy--process candidates
;;                             (avy--style-fn avy-style)))))

(defun idiig/lsp-avy-goto-word ()
  (interactive)
  (idiig//lsp-avy-document-symbol t))

(defun idiig/lsp-avy-goto-symbol ()
  (interactive)
  (idiig//lsp-avy-document-symbol nil))

;; REPL
(defvar idiig-repl-list '()
  "List of all registered REPLs.")

(defun idiig/register-repl (feature repl-func &optional tag)
  "Register REPL-FUNC to the global list of REPLs SPACEMACS-REPL-LIST.
FEATURE will be loaded before running the REPL, in case it is not already
loaded. If TAG is non-nil, it will be used as the string to show in the helm
buffer."
  (push `(,(or tag (symbol-name repl-func))
          . (,feature . ,repl-func))
        idiig-repl-list))

;; run-prog-mode-hook
(defun idiig/run-prog-mode-hooks ()
  "Runs `prog-mode-hook'. Useful for modes that don't derive from
`prog-mode' but should."
  (run-hooks 'prog-mode-hook))

(provide 'idiig-prog)
