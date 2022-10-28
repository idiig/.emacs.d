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
  (progn
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
          (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.~undo-tree~$"))

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
    (evil-define-key 'normal dired-mode-map "J" 'find-file)
    ;; (evil-define-key 'normal dired-mode-map "f" 'idiig/open-file-with-projectile-or-counsel-git)
    (evil-define-key 'normal dired-mode-map ")" 'dired-omit-mode)
    (evil-define-key 'normal dired-mode-map "q" 'quit-window)
    (defvar dired-filelist-cmd
      '(("vlc" "-L")))))

(provide 'idiig-dired)
