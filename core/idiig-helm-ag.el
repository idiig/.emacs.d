;; helm ag for 项目和文档检索
;; occur
(evil-set-initial-state 'occur-mode 'normal)
(with-eval-after-load 'occur-edit-mode
  (define-key 'normal occur-edit-mode-map (kbd "C-c C-c") 'occur-cease-edit))

(use-package helm-ag
  :commands (idiig/helm-files-smart-do-search
             idiig/helm-files-smart-do-search-region-or-symbol
             idiig/helm-files-do-ag
             idiig/helm-files-do-ag-region-or-symbol
             idiig/helm-buffers-smart-do-search
             idiig/helm-buffers-smart-do-search-region-or-symbol
             idiig/helm-buffers-do-ag
             idiig/helm-buffers-do-ag-region-or-symbol)
  :custom (helm-minibuffer-history-key "M-p")
  :init
  (progn
    (add-to-list 'exec-path "/usr/local/bin")   ;; 添加 ag 引擎路径
    (use-package pcre2el)                       ;; 函数 rxt-quote-pcre 需要
    (use-package projectile)                    ;; project管理
    (defvar idiig-search-tools '("ag"))         ;; 检索工具设置为ag。可添加，添加时需要增加之后的函数
    ;; (which-key-declare-prefixes "C-SPC sa" "ag")
    ;; 激活选择范围
    (defun idiig//helm-do-ag-region-or-symbol (func &optional dir)
      (require 'helm-ag)
      (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
                 ;; make thing-at-point choosing the active region first
                 ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
                 ((symbol-function 'thing-at-point)
                  (lambda (thing)
                    (let ((res (if (region-active-p)
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end))
                                 (this-fn thing))))
                      (when res (rxt-quote-pcre res))))))
        (funcall func dir)))

    ;; 结合多个参数函数时需要的部分
    (defun idiig//helm-do-search-find-tool (base tools default-inputp)
      (eval
       `(cond
         ,@(mapcar
            (lambda (x)
              `((executable-find ,x)
                ',(let ((func
                         (intern
                          (format (if default-inputp
                                      "idiig/%s-%s-region-or-symbol"
                                    "idiig/%s-%s")
                                  base x))))
                    (if (fboundp func)
                        func
                      (intern (format "%s-%s"  base x))))))
            tools)
         (t 'helm-do-grep))))

    ;; ---选择在任意文档中检索---
    ;; 选择检索文档
    (defun idiig/helm-files-do-ag (&optional dir)
      (interactive)
      (helm-do-ag dir))

    ;; 结合文档检索和字符串检索
    (defun idiig/helm-files-do-ag-region-or-symbol ()
      (interactive)
      (idiig//helm-do-ag-region-or-symbol 'idiig/helm-files-do-ag))

    ;; 检索栏中自动输入当前字串
    (defun idiig/helm-files-smart-do-search (&optional default-inputp)
      (interactive)
      (call-interactively
       (idiig//helm-do-search-find-tool "helm-files-do"
                                        idiig-search-tools
                                        default-inputp)))

    ;; 直接检索当前字串 
    (defun idiig/helm-files-smart-do-search-region-or-symbol ()
      (interactive)
      (idiig/helm-files-smart-do-search t))
    
    ;; ---选择在任意buffer中检索---
    ;; 选择检索buffer
    (defun idiig/helm-buffers-do-ag (&optional _)
      (interactive)
      (helm-do-ag-buffers))

    ;; 结合文档检索和字符串检索
    (defun idiig/helm-buffers-do-ag-region-or-symbol ()
      (interactive)
      (idiig//helm-do-ag-region-or-symbol 'idiig/helm-buffers-do-ag))

    ;; 检索栏中自动输入当前字串
    (defun idiig/helm-buffers-smart-do-search (&optional default-inputp)
      (interactive)
      (call-interactively
       (idiig//helm-do-search-find-tool "helm-buffers-do"
                                        idiig-search-tools
                                        default-inputp)))

    ;; 直接检索当前字串 
    (defun idiig/helm-buffers-smart-do-search-region-or-symbol ()
      (interactive)
      (idiig/helm-buffers-smart-do-search t))


    ;; ---选择在当前project中检索---
    ;; 检索project
    (defun idiig/helm-project-do-ag ()
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
            (helm-do-ag dir)
          (message "not in a project."))))

    ;; 结合project检索和字符串检索
    (defun idiig/helm-project-do-ag-region-or-symbol ()
      (interactive)
      (let ((dir (projectile-project-root)))
        (if dir
            (idiig//helm-do-ag-region-or-symbol 'helm-do-ag dir)
          (message "not in a project."))))

    ;; 检索栏中自动输入当前字串
    (defun idiig/helm-project-smart-do-search (&optional default-inputp)
      (interactive)
      (let ((projectile-require-object-root nil))
        (call-interactively
         (idiig//helm-do-search-find-tool "helm-project-do"
                                          idiig-search-tools
                                          default-inputp))))

    ;; 直接检索当前字串 
    (defun idiig/helm-project-smart-do-search-region-or-symbol ()
      (interactive)
      (idiig/helm-project-smart-do-search t))

    ;;----projectile 设置
    (with-eval-after-load 'helm-projectile
      (defun idiig/helm-project-smart-do-search-in-dir (dir)
        (interactive)
        (let ((default-directory dir))
          (idiig/helm-project-smart-do-search)))
      (define-key helm-projectile-projects-map
        (kbd "C-s")
        (lambda ()
          (interactive)
          (helm-exit-and-execute-action
           'idiig/helm-project-smart-do-search-in-dir))))

    ;; ;; -----键位设置
    ;; (evil-leader/set-key
    ;;   ;; opened buffers scope
    ;;   "sb"  'idiig/helm-buffers-smart-do-search
    ;;   "sB"  'idiig/helm-buffers-smart-do-search-region-or-symbol
    ;;   "sab" 'helm-do-ag-buffers
    ;;   "saB" 'idiig/helm-buffers-do-ag-region-or-symbol
    ;;   ;; files scope
    ;;   "sf"  'idiig/helm-files-smart-do-search
    ;;   "sF"  'idiig/helm-files-smart-do-search-region-or-symbol
    ;;   "saf" 'helm-do-ag
    ;;   "saF" 'idiig/helm-files-do-ag-region-or-symbol
    ;;   ;; current project scope
    ;;   "/"   'idiig/helm-project-smart-do-search
    ;;   "*"   'idiig/helm-project-smart-do-search-region-or-symbol
    ;;   "sp"  'idiig/helm-project-smart-do-search
    ;;   "sP"  'idiig/helm-project-smart-do-search-region-or-symbol
    ;;   "sap" 'idiig/helm-project-do-ag
    ;;   "saP" 'idiig/helm-project-do-ag-region-or-symbol)
    )
  :config
  (progn
    ;; for helm ag
    (defvar-local idiig--gne-min-line nil
      "The first line in the buffer that is a valid result.")
    (defvar-local idiig--gne-max-line nil
      "The last line in the buffer that is a valid result.")
    (defvar-local idiig--gne-cur-line 0
      "The current line in the buffer. (It is problematic to use
point for this.)")
    (defvar-local idiig--gne-line-func nil
      "The function to call to visit the result on a line.")

    (defun idiig/gne-next (num reset)
      (when reset (setq idiig--gne-cur-line
                        idiig--gne-min-line))
      (setq idiig--gne-cur-line
            (min idiig--gne-max-line
                 (max idiig--gne-min-line
                      (+ num idiig--gne-cur-line))))
      (goto-line idiig--gne-cur-line)
      (funcall idiig--gne-line-func
               (buffer-substring (point-at-bol) (point-at-eol))))

    (defun idiig//gne-init-helm-ag (&rest args)
      (with-current-buffer "*helm ag results*"
        (setq idiig--gne-min-line 5
              idiig--gne-max-line (save-excursion
                                    (goto-char (point-max))
                                    (previous-line)
                                    (line-number-at-pos))
              idiig--gne-line-func
              (lambda (c)
                (helm-ag--find-file-action
                 c 'find-file helm-ag--search-this-file-p))
              next-error-function 'idiig/gne-next)))
    (advice-add 'helm-ag--save-results :after 'idiig//gne-init-helm-ag)
    
    ;; ag检索和编辑保持出现在屏幕右侧
    (defun idiig-ag-edit (function)
      (when (get-buffer "*helm-ag-edit*")
        (kill-buffer "*helm-ag-edit*"))
      (if (not (= (count-windows) 2))
          (progn
            (split-window-right))))

    (advice-add 'helm-ag--edit :before #'idiig-ag-edit)
    ;; (advice-add 'helm-ag--edit :after #'idiig-after-ag-edit)

    ;; helm保持出现在下方
    (defvar helm-position 'bottom)
    (defvar idiig-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
    (defvar idiig-helm-display-buffer-regexp
      `("*.*helm.**"
        (display-buffer-in-side-window)
        (inhibit-same-window . t)
        (side . ,helm-position)
        (window-width . 0.6)
        (window-height . 0.4)))
    (defun display-helm-at-bottom (buffer &optional _resume)
      (let ((display-buffer-alist (list idiig-helm-display-help-buffer-regexp
                                        idiig-helm-display-buffer-regexp)))
        (display-buffer buffer)))
    (setq helm-display-function 'display-helm-at-bottom)

    (evil-define-key 'normal helm-ag-map "SPC" (make-sparse-keymap))
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    ;; evilify the helm-grep buffer
    ;; (evilified-state-evilify helm-grep-mode helm-grep-mode-map
    ;;   (kbd "RET") 'helm-grep-mode-jump-other-window
    ;;   (kbd "q") 'quit-window)
    ;; (evilified-state-evilify helm-ag-mode helm-ag-mode-map
    ;;   (kbd "RET") 'helm-ag-mode-jump-other-window
    ;;   (kbd "gr") 'helm-ag--update-save-results
    ;;   (kbd "q") 'quit-window)
    ))

(provide 'idiig-helm-ag)
