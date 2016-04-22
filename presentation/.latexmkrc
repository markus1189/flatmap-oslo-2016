$latex = 'informative latex -interaction batchmode -halt-on-error -synctex=1 -shell-escape %O %S';
$pdflatex = 'pdflatex -interaction batchmode -halt-on-error -synctex=1 --shell-escape %O %S';

# 1 = pdflatex, 2 = eps2pdf, 3 = dvi2pdf
$pdf_mode = 1;

$cleanup_mode = 2;
$clean_ext = "bbl";

$max_repeat = 7;

$preview_continuous_mode = 0;
