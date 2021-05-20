(TeX-add-style-hook
 "draft"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "autodetect-engine" "dvi=dvipdfmx" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("ulem" "normalem") ("datetime" "yyyymmdd") ("biblatex" "natbib" "style=apa" "backend=biber" "giveninits=false" "")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "geometry"
    "fontspec"
    "xeCJK"
    "graphicx"
    "longtable"
    "float"
    "wrapfig"
    "rotating"
    "ulem"
    "textcomp"
    "multicol"
    "amsmath"
    "amsthm"
    "amssymb"
    "bm"
    "booktabs"
    "url"
    "csquotes"
    "tikz"
    "pgfplots"
    "svg"
    "minted"
    "datetime"
    "xcolor"
    "hyperref"
    "pxjahyper"
    "biblatex"
    "etoolbox"
    "footmisc"
    "fancyhdr"
    "capt-of"
    "caption"
    "subcaption"
    "framed")
   (TeX-add-symbols
    "FrameCommand"))
 :latex)

