(use-package python
  :defer t 
  :init  
  (progn
    ;; 缩进
    (setq python-indent-offset 4)
    (idiig/register-repl 'python-mode 'python)))

(with-eval-after-load 'python
  (setq
   python-shell-interpreter "ipython3"
   python-shell-interpreter-args "--simple-prompt"
   org-babel-python-command '"ipython --simple-prompt")
  ;; 虚拟环境
  (use-package pyvenv
    :commands (pyvenv-workon pyvenv-activate)
    :init
    (evil-leader/set-key-for-mode 'python-mode "v" 'pyvenv-workon)
    (setenv "WORKON_HOME" "~/.pyenv/versions"))
  
  ;; 测试 >>??
  (use-package pytest
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-last-failed
               pytest-pdb-last-failed
               pytest-module
               pytest-pdb-module)
    :init
    (evil-leader/set-key-for-mode 'python-mode
      "tA" 'pytest-pdb-all
      "ta" 'pytest-all
      "tO" 'pytest-pdb-one
      "to" 'pytest-one
      "tM" 'pytest-pdb-module
      "tm" 'pytest-module
      "tD" 'pytest-directory)
    :config
    (add-to-list 'pytest-project-root-files "setup.cfg"))

  ;; 代码整形
  (use-package yapfify
    :defer t
    :init
    (progn
      (add-hook 'python-mode-hook 'yapf-mode)
      (evil-leader/set-key-for-mode 'python-mode "==" 'yapfify-buffer))))


(provide 'idiig-python)
