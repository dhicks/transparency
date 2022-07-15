all: pdf

pdf: paper.pdf
paper.pdf: paper.Qmd \
           ../scripts/03_dag.html
	quarto render paper.Qmd --to pdf

html: paper.html
paper.html: paper.Qmd \
            ../scripts/03_dag.html
	quarto render paper.Qmd --to html
