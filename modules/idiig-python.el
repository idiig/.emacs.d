(use-package python
  :defer t 
  :init  
  (progn
    ;; long quotes
    (defun python-electric-pair-string-delimiter ()
      (when (and electric-pair-mode
                 (memq last-command-event '(?\" ?\'))
                 (let ((count 0))
                   (while (eq (char-before (- (point) count)) last-command-event)
                     (setq count (1+ count)))
                   (= count 3)))
        (save-excursion (insert (make-string 3 last-command-event)))))
    (add-hook 'python-hook
              (lambda ()
                (add-hook 'post-self-insert-hook
                          #'python-electric-pair-string-delimiter 'append t)))
    (idiig/register-repl 'python-mode 'python)))

(with-eval-after-load 'python
  (setq
   python-shell-interpreter "ipython3"
   python-shell-interpreter-args "-i")
  ;; 虚拟环境
  (use-package pyvenv
    :commands (pyvenv-workon pyvenv-activate)
    :init
    (evil-leader/set-key-for-mode 'python-mode "v" 'pyvenv-workon)
    (setenv "WORKON_HOME" "~/anaconda3/envs"))

  ;; 缩进
  (setq python-indent-offset 4)
  
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
    (which-key-declare-prefixes-for-mode 'python-mode ",t" "pytest")
    (which-key-declare-prefixes-for-mode 'python-mode "SPC mt" "pytest")
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
