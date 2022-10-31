;; global set key
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-x C-r") 'recent-open-file)
(global-set-key (kbd "C-h f") 'describe-function)
(global-set-key (kbd "C-h v") 'describe-variable)
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-s-'") 'avy-goto-char-2)
(global-set-key (kbd "M-'") 'avy-goto-char-2)
(global-set-key (kbd "C-M-/") 'fci-mode)  ;; 80字提示线
;; (bind-key* "C-c k" 'which-key-show-top-level)  ;; which-key top level
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line) ;; 跳到代码最前
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)  ;; 跳到代码最后
(global-set-key (kbd "C-=") 'er/expand-region) ;; 扩大选取
(global-set-key (kbd "C--") 'er/contract-region)  ;; 缩小选取

;; 自定义功能 
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)  ;; 全局自动缩进
(global-set-key (kbd "C-M-¥") 'indent-region-or-buffer)  ;; 全局自动缩进
;; (global-set-key (kbd "C-c C-e") 'occur-dwim)  
(global-set-key (kbd "C-w") 'backward-kill-word-or-region);; 删除word 
(bind-key* "C-." 'idiig/insert-space-after-point) ;; 点后插入空白
(global-set-key [(shift return)] 'idiig/smart-open-line) ;; 行末尾加白行
(bind-key* "M--" 'idiig/goto-match-paren)  ;; 到匹配括号
(global-set-key (kbd "<f5>") 'idiig/run-current-file)  ;; 跑程序

;; hydra-buffer
(defhydra hydra-buffer (:color red
                               :hint nil)
  "
buffer: _n_ext  _p_rivious"
  ("n" next-buffer)
  ("p" previous-buffer))

;; hydra-window
(defhydra hydra-window (:color red
                               :hint nil)
  "
window: _[_:shrink _]_:enlarge _=_:balance"
  ("[" idiig/shrink-window-horizontally)
  ("]" idiig/enlarge-window-horizontally)
  ("=" balance-windows-area))

;; WhichKeyの古い関数をSpacemacsで使っている
;; https://note.com/5mingame2/n/n2e2872ad1384
(defalias 'which-key-declare-prefixes 'which-key-add-key-based-replacements)
(make-obsolete 'which-key-declare-prefixes
              'which-key-add-key-based-replacements
              "2016-10-05")
(defalias 'which-key-declare-prefixes-for-mode
 'which-key-add-major-mode-key-based-replacements)
(make-obsolete 'which-key-declare-prefixes-for-mode
              'which-key-add-major-mode-key-based-replacements
              "2016-10-05")

;; 给 major mode 定义 leader key
(defvar idiig-leader-key "SPC"
  "The leader key.")

(defvar idiig-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar idiig-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar idiig-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

;; 激活leader-key
(use-package general :ensure t)
(general-define-key
 :keymaps '(normal visual emacs motion)
 :prefix idiig-leader-key
 :non-normal-prefix "C-SPC"
 "" nil
 ;; (evil-leader/set-key ;; instead by general-define-key
 "<SPC>" 'execute-extended-command
 "TAB" 'idiig/alternate-buffer
 ;;---file
 "ff" 'find-file  ;; Use consult
 "fr" 'consult-recent-file
 "fD" 'delete-current-buffer-file
 ;;---buffer
 "bb" 'switch-to-buffer ;;consult-buffer
 "bd" 'kill-this-buffer
 "be" 'eval-buffer
 "bp" 'hydra-buffer/previous-buffer
 "bn" 'hydra-buffer/next-buffer
 "bx" 'kill-buffer-and-window
 "bD" 'kill-other-buffers
 "b TAB" 'idiig/alternate-buffer
 "b." 'hydra-buffer/body
 "fR" 'idiig/rename-file-and-buffer
 ;;---window
 "w/" 'split-window-right
 "w-" 'split-window-below
 "w[" 'hydra-window/idiig/shrink-window-horizontally
 "w]" 'hydra-window/idiig/enlarge-window-horizontally
 "w=" 'balance-windows-area
 "wh" 'evil-window-left
 "wH" 'evil-window-move-far-left
 "wj" 'evil-window-down
 "wJ" 'evil-window-move-very-bottom
 "wk" 'evil-window-up
 "wK" 'evil-window-move-very-top
 "wl" 'evil-window-right
 "wm" 'maximize-window
 "wd" 'delete-window
 "wx" 'kill-buffer-and-window
 "w TAB" 'idiig/alternate-window
 "w." 'hydra-window/body
 ;;---window select
 "1" 'winum-select-window-1
 "2" 'winum-select-window-2
 "3" 'winum-select-window-3
 "4" 'winum-select-window-4
 ;;---jump avy
 "jb" 'avy-pop-mark
 "jj" 'evil-avy-goto-char-timer
 "jl" 'evil-avy-goto-line
 ;; "ju" 'idiig/avy-goto-url
 "jw" 'evil-avy-goto-word-or-subword-1
 ;; "jo" 'idiig/avy-open-url
 ;;---search and edit
 "ss" 'idiig/consult-buffer-region-or-symbol
 "/"   'consult-ripgrep
 "*"   'idiig/consult-project-region-or-symbol
 "sp"  'consult-ripgrep
 "sP"  'idiig/consult-project-region-or-symbol
 ;;---org global
 ;; Add global evil-leader mappings. Used to access org-agenda
 ;; functionalities – and a few others commands – from any other mode.
 ;; org-agenda
 "o/" 'org-occur-in-agenda-files
 "o#" 'org-agenda-list-stuck-projects
 "oa" 'org-agenda-list
 "oo" 'org-agenda
 "oc" 'org-capture
 "oe" 'org-store-agenda-views
 "ofi" 'org-feed-goto-inbox
 "ofu" 'org-feed-update-all
 ;; Clock
 ;; These keybindings should match those under the "mC" prefix (above)
 "oCc" 'org-clock-cancel
 "oCg" 'org-clock-goto
 "oCi" 'org-clock-in
 "oCI" 'org-clock-in-last
 "oCj" 'org-clock-jump-to-current-clock
 "oCo" 'org-clock-out
 "oCr" 'org-resolve-clocks
 ;;
 "ol" 'org-store-link
 "om" 'org-tags-view
 "os" 'org-search-view
 "ot" 'org-todo-list
 "op" 'org-pomodoro)

;; ;; 提示快捷键
;; (which-key-declare-prefixes "SPC SPC" "M-x")
;; (which-key-declare-prefixes "SPC TAB" "last buffer")
;; (which-key-declare-prefixes "SPC /" "quick search project")
;; (which-key-declare-prefixes "SPC *" "quick search project input")
;; (which-key-declare-prefixes "SPC b" "buffers")
;; (which-key-declare-prefixes "SPC e" "eval")
;; (which-key-declare-prefixes "SPC f" "files")
;; (which-key-declare-prefixes "SPC j" "avy-jump")
;; (which-key-declare-prefixes "SPC s" "search")
;; (which-key-declare-prefixes "SPC w" "windows")
;; (which-key-declare-prefixes "SPC o" "org")
;; (which-key-declare-prefixes "SPC of" "feeds")
;; (which-key-declare-prefixes "SPC oC" "org-clocks")
;; (which-key-declare-prefixes "C-SPC SPC" "M-x")
;; (which-key-declare-prefixes "C-SPC TAB" "last buffer")
;; (which-key-declare-prefixes "C-SPC /" "quick search project")
;; (which-key-declare-prefixes "C-SPC *" "quick search project input")
;; (which-key-declare-prefixes "C-SPC b" "buffers")
;; (which-key-declare-prefixes "C-SPC e" "eval")
;; (which-key-declare-prefixes "C-SPC f" "files")
;; (which-key-declare-prefixes "C-SPC j" "avy-jump")
;; (which-key-declare-prefixes "C-SPC s" "search")
;; (which-key-declare-prefixes "C-SPC sa" "ag")
;; (which-key-declare-prefixes "C-SPC w" "windows")
;; (which-key-declare-prefixes "C-SPC o" "org")
;; (which-key-declare-prefixes "C-SPC of" "feeds")
;; (which-key-declare-prefixes "C-SPC oC" "org-clocks")

(provide 'idiig-global-keybindings)
