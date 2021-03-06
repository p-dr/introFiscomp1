(TeX-add-style-hook
 "relatorio"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "inputenc"
    "graphicx"
    "listings"
    "amsmath")
   (LaTeX-add-labels
    "eq:vel"
    "eq:2"
    "eq:3"
    "eq:1")))

