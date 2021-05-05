;; 加速
(setq gc-cons-threshold (* 2 1000 1000))
(setq gc-cons-threshold (* 50 1000 1000))

;; 保持更新代码
(setq load-prefer-newer t)

;; 定义文件结构
(defvar idiig-dir (file-name-directory load-file-name))             ;; 当前文件夹为idiig-dir
(defvar idiig-core-dir (expand-file-name "core" idiig-dir))         ;; core文件夹为idiig-core-dir
(defvar idiig-modules-dir (expand-file-name  "modules" idiig-dir))  ;; modules文件夹为idiig-modules-dir
(defvar idiig-modules-dir (expand-file-name  "dependencies" idiig-dir))  ;; modules文件夹为idiig-dependencies-dir

;; 隐藏文件夹
(defvar idiig-cache-dir (expand-file-name  ".cache" idiig-dir))      ;; 用户数据文件夹
(defvar idiig-savefile-dir (expand-file-name "savefile" idiig-cache-dir)) ;; 自动保存文件夹

;; 添加到加载路径
(add-to-list 'load-path idiig-core-dir)
(add-to-list 'load-path idiig-modules-dir)

;; 包设定
(require 'package)
(setq package-user-dir (expand-file-name ".elpa" idiig-dir))
(package-initialize)
;; 源设定
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("org" . "http://orgmode.org/elpa/")))
;; use-package
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer nil)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)
;; use-package的附加
(use-package quelpa-use-package) ;; 如果melpa里没有包从github下载
(use-package diminish) ;; 关闭一些mode提示

;; 导入核心模块
(require 'idiig-ui)
(require 'idiig-evil)
(require 'idiig-better-default)
(require 'idiig-better-search)                         ;; 项目，文档检索和管理
(require 'idiig-auto-complete)                         ;; 自动补全
(require 'idiig-global-keybindings)

;; 导入功能模块
(require 'idiig-japanese)
(require 'idiig-prog)
(require 'idiig-org)
(require 'idiig-git)
(require 'idiig-tex)
(require 'idiig-python)
(require 'idiig-ess)

;; custom文件
(setq custom-file (expand-file-name "custom.el" idiig-core-dir))
(load-file custom-file)
