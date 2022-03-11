;; 基础设置
(tool-bar-mode -1) ;; 关闭工具栏
(menu-bar-mode -1) ;; 关闭shell的工具栏
(scroll-bar-mode -1) ;; 关闭文件滑动控件
(global-linum-mode -1) ;; 不显示行号
(setq cursor-type 'bar) ;; 更改光标的样式（不能生效，解决方案见第二集）
(setq inhibit-splash-screen 1) ;; 关闭启动帮助画面
(set-face-attribute 'default nil :height 160) ;; 更改显示字体大小 16pt
(setq initial-frame-alist (quote ((fullscreen . maximized)))) ;; 全屏
(setq initial-major-mode 'text-mode) ;; scratch为草稿
(setq initial-scratch-message nil) ;; 关闭scratch message
(setq initial-buffer-choice  nil) ;; 关闭 buffer choice
(setq inhibit-startup-message t) ;; 关闭启动信息
(setq ns-use-proxy-icon nil)  ;; 删除frame icon
(setq-default line-spacing 1)   ;; 调整行高
(setq frame-title-format
      ;; 窗口显示文件路径/buffer名
      '("" " idiig - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))
;; (require 'hl-line)
;; (defun global-hl-line-timer-function ()
;;   ;; 一定时间后后高亮所在行
;;   (global-hl-line-unhighlight-all)
;;   (let ((global-hl-line-mode t))
;;     (global-hl-line-highlight)))
;; (setq global-hl-line-timer
;;       ;; 30s后高亮所在行
;;       (run-with-idle-timer 10.00 t 'global-hl-line-timer-function))
;; ;; (cancel-timer global-hl-line-timer)

;; Theme
;; (load-theme 'leuven t)
(use-package parchment-theme
  :config (load-theme 'parchment t))

;; mode-line
(use-package doom-modeline
  :custom
  (doom-modeline-env-version nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-project-detection 'project)
  (doom-modeline-workspace-name nil)
  :config
  (progn
    (doom-modeline-mode 1)
    (line-number-mode 1)
    (column-number-mode 0)))

;; 字体调整
(cond ((display-graphic-p) ;; 避免终端打开时字体错误
       (progn
         ;; 统一拉丁字母和汉CJK
         ;; http://chenzaichun.github.io/2011-12-30-mac-os-x-emacs-chinese-font-setting.html
	 (let ((my-font-height 200)
               (my-font
                ;; "Menlo"
                ;; "Noto Sans Mono"
                "Sarasa Mono SC Nerd"
                )
               (my-font-ja
                ;; "STHeiti"
                ;; "Takaoゴシック-10"
                ;; "hiragino Kaku Gothic ProN"
                ;; "微软雅黑"
                ;; "Noto Sans"
                "Sarasa Mono SC Nerd"
                ))
           (setq mac-allow-anti-aliasing t)
           (setq face-font-rescale-alist
		 '(("^-apple-hiragino.*" . 1.2)
                   ;; (".*Hiragino Kaku Gothic ProN.*" . 1.6)
                   (".*osaka-bold.*" . 1.2)
                   (".*osaka-medium.*" . 1.2)
                   (".*courier-bold-.*-mac-roman" . 1.0)
                   (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                   (".*monaco-bold-.*-mac-roman" . 0.9)
                   ("-cdac$" . 1.3)))
           (when my-font
             (set-face-attribute 'default nil :family my-font :height my-font-height)
             )
           (when my-font-ja
             (let ((fn (frame-parameter nil 'font))
                   (rg "iso10646-1"))
               (set-fontset-font fn 'chinese-gb2312 `(,my-font-ja . ,rg))
               (set-fontset-font fn 'chinese-gbk `(,my-font-ja . ,rg)))))
	 )))

;; 80字数提示线
(use-package fill-column-indicator
  :commands (fci-mode)
  :init
  (setq-default fci-rule-column 80) ; 縦線の幅
  (setq fci-rule-width 1)
  (setq fci-rule-color "gray30") ; 縦線の色
  )

(provide 'idiig-ui)
