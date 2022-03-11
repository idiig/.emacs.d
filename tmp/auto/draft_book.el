(TeX-add-style-hook
 "draft_book"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("scrreprt" "autodetect-engine" "dvi=dvipdfmx" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("ulem" "normalem") ("datetime" "yyyymmdd") ("biblatex" "natbib" "style=apa" "backend=biber" "giveninits=false" "") ("cleveref" "capitalize" "noabbrev")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "scrreprt"
    "scrreprt11"
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
    "appendix"
    "minted"
    "csquotes"
    "tikz"
    "pgfplots"
    "svg"
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
    "framed"
    "cleveref")
   (TeX-add-symbols
    "FrameCommand"))
 :latex)

