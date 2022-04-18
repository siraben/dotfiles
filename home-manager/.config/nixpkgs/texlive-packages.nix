{ pkgs }:

pkgs.texlive.combine {
  inherit (pkgs.texlive)
    scheme-minimal # base
    amsmath # math symbols
    beamer # beamer
    biblatex # citations
    capt-of
    catchfile
    cm-super # vectorized fonts
    collection-latex # pdflatex
    csquotes
    dvipng
    framed
    fvextra
    latexmk
    lkproof # proof rules
    minted # source code 
    preprint
    rotfloat
    ulem
    upquote
    wrapfig
    xstring
    endnotes
    caption
  ;
}
