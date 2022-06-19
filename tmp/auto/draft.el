(TeX-add-style-hook
 "draft"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "autodetect-engine" "dvi=dvipdfmx" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("ulem" "normalem") ("datetime" "yyyymmdd") ("cleveref" "capitalize" "noabbrev")))
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
    "unicode-math"
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

