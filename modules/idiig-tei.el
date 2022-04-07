(use-package nxml-mode
  :mode
  (("\.xml$" . nxml-mode)
   ("\.xsl$" . nxml-mode)
   ("\.xhtml$" . nxml-mode)
   ("\.page$" . nxml-mode))
  :config
  (setq nxml-child-indent 2)                  ; タグのインデント幅
  (setq nxml-attribute-indent 2)              ; 属性のインデント幅
  (setq indent-tabs-mode nil)
  (setq nxml-slash-auto-complete-flag t)      ; </の入力で閉じタグを補完する
  (setq tab-width 2))

(provide 'idiig-tei)
