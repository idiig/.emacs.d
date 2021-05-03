(use-package company
  :diminish company-mode
  :config
  ;; 防止冲突
  (defvar-local company-fci-mode-on-p nil)
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))
  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))
  :init
  (progn
    (setq company-idle-delay 0.1
          company-show-numbers t
          company-minimum-prefix-length 2
          company-tooltip-limit 10 
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-tooltip-flip-when-above t
          company-tooltip-align-annotations t)
    (add-hook 'company-completion-started-hook 'company-turn-off-fci)      ;;解决一些冲突，下面同样
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
  :config
  (progn
    (let ((map company-active-map))
      (define-key map [return]      'company-complete-selection)
      (define-key map (kbd "RET")   'company-complete-selection)
      (define-key map (kbd "TAB")   'company-complete-common-or-cycle)
      (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
      (define-key map (kbd "C-/")   'company-search-candidates)
      (define-key map (kbd "C-M-/") 'company-filter-candidates)
      (define-key map (kbd "C-d")   'company-show-doc-buffer)
      (define-key map (kbd "C-j")   'company-select-next)
      (define-key map (kbd "C-k")   'company-select-previous)
      (define-key map (kbd "C-l")   'company-complete-selection))
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")
    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
    (global-company-mode t)
    ;; company box
    (use-package company-box
      :diminish
      :custom
      (company-box-doc-enable nil)
      (company-box-enable-icon nil)
      (company-box-backends-colors nil)
      (company-box-show-single-candidate t)
      (company-box-max-candidates 50)
      :config
      ;; 使用company-box来写org的话，补全选项会更好看一些
      (add-hook 'org-mode-hook 'company-box-mode))))

;; ;; 杂乱的字符也可以检索
;; (use-package company-fuzzy
;;   :diminish company-fuzzy-mode
;;   :config
;;   (global-company-fuzzy-mode 1))

;; (use-package fuzzy :defer t)

;; 自動弹出文档信息
(use-package company-quickhelp
  :commands company-quickhelp-manual-begin
  :init
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
    (company-quickhelp-mode)))

;; hippie-expand补齐
(use-package hippie-exp
  :init
  (progn
    (global-set-key (kbd "M-/") 'hippie-expand)
    (define-key evil-insert-state-map [remap evil-complete-previous] 'hippie-expand)
    (setq hippie-expand-try-functions-list
          '(
            ;; Try to expand word "dynamically", searching the current buffer.
            try-expand-dabbrev
            ;; Try to expand word "dynamically", searching all other buffers.
            try-expand-dabbrev-all-buffers
            ;; Try to expand word "dynamically", searching the kill ring.
            try-expand-dabbrev-from-kill
            ;; Try to complete text as a file name, as many characters as unique.
            try-complete-file-name-partially
            ;; Try to complete text as a file name.
            try-complete-file-name
            ;; Try to expand word before point according to all abbrev tables.
            try-expand-all-abbrevs
            ;; Try to complete the current line to an entire line in the buffer.
            try-expand-list
            ;; Try to complete the current line to an entire line in the buffer.
            try-expand-line
            ;; Try to complete as an Emacs Lisp symbol, as many characters as
            ;; unique.
            try-complete-lisp-symbol-partially
            ;; Try to complete word as an Emacs Lisp symbol.
            try-complete-lisp-symbol))
    ;; Try to expand yasnippet snippets based on prefix
    (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    ))

;; 根据出现频率提示
(use-package company-statistics
  :defer t
  :init
  (progn
    (setq company-statistics-file (expand-file-name "company-statistics-cache.el"
                                                    idiig-cache-dir))
    (add-hook 'company-mode-hook 'company-statistics-mode)))

;; auto-complete
(use-package auto-complete
  :defer t
  :init
  (setq ac-auto-start 0
        ;; ac-delay auto-completion-idle-delay
        ac-quick-help-delay 1.
        ac-use-fuzzy t
        ac-fuzzy-enable t
        ac-comphist-file (concat idiig-cache-dir "ac-comphist.dat")
        ;; use 'complete when auto-complete is disabled
        tab-always-indent 'complete
        ac-dwim t)
  :config
  (progn
    (require 'auto-complete-config)
    (setq-default ac-sources '(ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers))
    (add-to-list 'ac-sources 'ac-source-yasnippet)
    (add-to-list 'completion-styles 'initials t)
    (define-key ac-completing-map (kbd "C-j") 'ac-next)
    (define-key ac-completing-map (kbd "C-k") 'ac-previous)
    (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
    ))

;; yasnippet
(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode yas-active-extra-mode)
  :diminish
  :init
  (progn
    ;; 没有undefine error
    (defvar yas-global-mode nil)
    (setq yas-triggers-in-field t
          yas-wrap-around-region t
          helm-yas-display-key-on-candidate t)
    ;; 多键位时给出补全项
    (setq yas-prompt-functions '(yas-completing-promptq))
    ;; minor mode键位重制
    (setq yas-minor-mode-map (make-sparse-keymap))
    (define-key yas-minor-mode-map (kbd "M-s /") 'yas-next-field)
    ;; yas的一些函数
    (defun idiig/load-yasnippet ()
      (unless yas-global-mode (yas-global-mode 1))
      (yas-minor-mode 1))
    (defun idiig/force-yasnippet-off ()
      (yas-minor-mode -1)
      (setq yas-dont-activate t))
    ;; hooks
    (add-hook 'term-mode-hook 'idiig/force-yasnippet-off)
    (add-hook 'shell-mode-hook 'idiig/force-yasnippet-off)
    (add-hook 'eshell-mode-hook 'idiig/force-yasnippet-off)
    (add-hook 'org-mode-hook 'idiig/load-yasnippet)
    (add-hook 'prog-mode-hook 'idiig/load-yasnippet)
    (add-hook 'markdown-mode-hook 'idiig/load-yasnippet))
  :config
  (use-package yasnippet-snippets)
  )
;; ;; yasnippet
;; (use-package yasnippet
;;   :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
;;   :init
;;   (progn
;;     (defvar yas-global-mode nil)
;;     (setq yas-triggers-in-field t
;;           yas-wrap-around-region t
;;           helm-yas-display-key-on-candidate t)
;;     (setq yas-prompt-functions '(yas-completing-prompt))
;;     (setq yas-minor-mode-map (make-sparse-keymap))
;;     (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
;;     (setq-yas-snippet-dirs
;;      '("~/.emacs.d/snippets/"
;;        "~/.emacs.d/snippet/"))
;;     ;; (spacemacs/add-to-hooks
;;     ;;  'spacemacs/force-yasnippet-off '(term-mode-hook
;;     ;;                                   shell-mode-hook
;;     ;;                                   eshell-mode-hook))
;;     ;; (spacemacs|require-when-dumping 'yasnippet)
;;     ;; (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
;;     ;;                                                     markdown-mode-hook
;;     ;;                                                     org-mode-hook))
;;     ))

;; https://emacs.stackexchange.com/a/7925/12854
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    ;; (indent-for-tab-command)
    (if (and (or (not yas-minor-mode)
                 (null (do-yas-expand)))
             (check-expansion))
        (progn
          (company-manual-begin)
          (if (null company-candidates)
              (progn
                (company-abort)
                (hippie-expand nil)
                ;; (indent-for-tab-command)
                )))
      ))))

;; (defun tab-complete-or-next-field ()
;;   (interactive)
;;   (if (or (not yas-minor-mode)
;;           (null (do-yas-expand)))
;;       (if company-candidates
;;           (company-complete-selection)
;;         (if (check-expansion)
;;             (progn
;;               (company-manual-begin)
;;               (if (null company-candidates)
;;                   (progn
;;                     (company-abort)
;;                     (yas-next-field))))
;;           (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas-minor-mode)
          (null (do-yas-expand))
          (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

(define-key evil-insert-state-map [tab] 'tab-indent-or-complete)
(define-key evil-insert-state-map (kbd "TAB") 'tab-indent-or-complete)

(define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
(define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)


(provide 'idiig-auto-complete)

