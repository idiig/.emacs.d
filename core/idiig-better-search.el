;; dired设置 
;; 获取文件大小
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(defun dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files
                 t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: "
                                current-prefix-arg files)
      files)))
  (let (list-switch)
    (start-process
     cmd nil shell-file-name
     shell-command-switch
     (format
      "nohup 1>/dev/null 2>/dev/null %s \"%s\""
      (if (and (> (length file-list) 1)
               (setq list-switch
                     (cadr (assoc cmd dired-filelist-cmd))))
          (format "%s %s" cmd list-switch)
        cmd)
      (mapconcat #'expand-file-name file-list "\" \"")))))

;; 用终端打开当前文件夹 
(defun dired-open-term ()
  "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let* ((current-dir (dired-current-directory))
         (buffer (if (get-buffer "*zshell*")
                     (switch-to-buffer "*zshell*")
                   (ansi-term "/bin/zsh" "zshell")))
         (proc (get-buffer-process buffer)))
    (term-send-string
     proc
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))

(defun dired-copy-file-here (file)
  (interactive "fCopy file: ")
  (copy-file file default-directory))

(defun my-dired-find-file ()
  "Open buffer in another window"
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (if (car (file-attributes filename))
        (dired-find-alternate-file)
      (dired-find-file-other-window))))

(defun idiig/dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

(defun idiig/dired-up-directory()
  "goto up directory and resue buffer"
  (interactive)
  (find-alternate-file ".."))

(use-package dired-mode
  :ensure nil
  :defer t
  :init
  (require 'dired-x) ;; 使用 C-x C-j 就可以进入当前文件夹的径路
  (require 'dired-aux)
  (setq dired-listing-switches "-alh")
  (setq dired-use-ls-dired nil) ;; on macOS, ls doesn't support the --dired
  (setq dired-recursive-deletes 'always) ;; 删除时不询问
  (setq dired-recursive-copies 'always)  ;; 复制时不询问
  (setq dired-dwin-target 1)  ;; 存在两个分屏时，将另一个分屏自动设置成拷贝地址的目标
  (put 'dired-find-alternate-file 'disabled nil)  ;; 不新打开buffer
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "open")
          ("\\.docx\\'" "open")
          ("\\.\\(?:djvu\\|eps\\)\\'" "open")
          ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
          ("\\.\\(?:xcf\\)\\'" "open")
          ("\\.csv\\'" "open")
          ("\\.tex\\'" "open")
          ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
           "open")
          ("\\.\\(?:mp3\\|flac\\)\\'" "open")
          ("\\.html?\\'" "open")
          ("\\.md\\'" "open")))
  ;; 无视某些文件
  (setq dired-omit-files
        (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))

  (defun ora-ediff-files ()
    ;; 比较两个文件
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "no more than 2 files should be marked"))))

  ;; key binding
  (evil-define-key 'normal dired-mode-map "e" 'ora-ediff-files)
  (evil-define-key 'normal dired-mode-map (kbd "C-k") 'idiig/dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "<RET>") 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "E" 'dired-toggle-read-only)
  (evil-define-key 'normal dired-mode-map "C" 'dired-do-copy)
  (evil-define-key 'normal dired-mode-map (kbd "<mouse-2>") 'my-dired-find-file)
  (evil-define-key 'normal dired-mode-map "`" 'dired-open-term)
  ;; (evil-define-key 'normal dired-mode-map "p" 'peep-dired-prev-file)
  ;; (evil-define-key 'normal dired-mode-map "n" 'peep-dired-next-file)
  (evil-define-key 'normal dired-mode-map "g" 'revert-buffer)
  (evil-define-key 'normal dired-mode-map "z" 'dired-get-size)
  (evil-define-key 'normal dired-mode-map "c" 'dired-copy-file-here)
  (evil-define-key 'normal dired-mode-map "J" 'counsel-find-file)
  ;; (evil-define-key 'normal dired-mode-map "f" 'idiig/open-file-with-projectile-or-counsel-git)
  (evil-define-key 'normal dired-mode-map ")" 'dired-omit-mode)
  ;; (evil-define-key 'normal dired-mode-map "q" 'quit-window)
  (defvar dired-filelist-cmd
    '(("vlc" "-L"))))

;; 搜索+集体修改
(use-package wgrep
  :init
  (evil-define-key 'motion wgrep-mode-map ",," 'wgrep-finish-edit)
  (evil-define-key 'motion wgrep-mode-map ",c" 'wgrep-finish-edit)
  (evil-define-key 'motion wgrep-mode-map ",a" 'wgrep-abort-changes)
  (evil-define-key 'motion wgrep-mode-map ",k" 'wgrep-abort-changes)

  (defun idiig/ivy-wgrep-change-to-wgrep-mode ()
    (interactive)
    (ivy-wgrep-change-to-wgrep-mode)
    (evil-normal-state))
  
  (defun idiig//counsel-edit ()
    "counsel检索时集体修改"
    (interactive)
    (run-with-idle-timer 0 nil 'idiig/ivy-wgrep-change-to-wgrep-mode)
    (ivy-occur))
  
  :config
  (setq wgrep-enable-key "e"))

;; occur
(evil-set-initial-state 'occur-mode 'normal)
(with-eval-after-load 'occur-edit-mode
  (define-key 'normal occur-edit-mode-map (kbd "C-c C-c") 'occur-cease-edit))

;; ivy
(use-package ivy
  :defer t
  :diminish 
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "[%d/%d] ")
  (ivy-use-virtual-buffers t)
  (ivy-height 15)
  (ivy-wrap t)
  :config
  (progn
    (add-to-list 'ivy-sort-functions-alist
                 '(counsel-recentf . file-newer-than-file-p))
    (setq enable-recursive-minibuffers t)
    (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "C-c C-e") 'idiig//counsel-edit)
    (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-previous-line)
    (ivy-mode)))

;; counsel
(use-package counsel
  :diminish
  :config (counsel-mode))

;; 让ivy给的选项更智能
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))

;; avy 转跳
(use-package avy
  :defer t
  :commands (idiig/avy-open-url idiig/avy-goto-url avy-pop-mark avy-with)
  :init
  (progn
    (setq avy-all-windows 'all-frames)
    (setq avy-background t)
    ;; (global-set-key (kbd "C-s-'") 'avy-goto-char-2)
    ;; (global-set-key (kbd "M-'") 'avy-goto-char-2)
    ;; (evil-leader/set-key
    ;;  "jb" 'avy-pop-mark
    ;;  "jj" 'evil-avy-goto-char-timer
    ;;  "jl" 'evil-avy-goto-line
    ;;  "ju" 'idiig/avy-goto-url
    ;;  "jU" 'idiig/avy-open-url
    ;;  "jw" 'evil-avy-goto-word-or-subword-1
    ;;  "jo" 'idiig/avy-open-url)
    ;; (which-key-declare-prefixes "SPC j" "avy-jump")
    )
  :config
  (progn
    (defun idiig/avy-goto-url()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy-jump "https?://"))
    (defun idiig/avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
        (idiig/avy-goto-url)
        (browse-url-at-point)))))

;; swiper for 文档检索
(use-package swiper
  :defer t
  :after ivy
  :init
  (defun idiig/swiper-region-or-symbol ()
    "检索当前字符或选中区域"
    (interactive)
    (let ((input (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'symbol t))))
      (swiper input)))
  ;; :config
  ;; (progn
  ;;   (evil-leader/set-key
  ;;     "ss" 'idiig/swiper-region-or-symbol)
  ;;   (global-set-key "\C-s" 'swiper))
  )

;; helm ag for 项目和文档检索
(use-package helm-ag
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

(provide 'idiig-better-search)
