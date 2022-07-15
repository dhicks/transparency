all: pdf

pdf: paper.pdf
paper.pdf: paper.Qmd
	quarto render paper.Qmd --to pdf

html: paper.html
paper.html: paper.Qmd
	quarto render paper.Qmd --to html

docx: paper.docx
paper.docx: paper.md
	pandoc header.yaml paper.md \
                    -o paper.docx \
                    --filter=pandoc-citeproc \
                    --filter=pandoc-crossref

