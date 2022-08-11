all: pdf docx

pdf: paper.pdf
paper.pdf: paper.Qmd \
           ../scripts/03_dag.html \
           fig2_part_values.png \
           fig3_shared_values_dag.png \
           fig4_conclusion_part.png \
           fig5_shared_part.png \
           fig6_sci_part.png
	quarto render paper.Qmd --to pdf --debug

html: paper.html
paper.html: paper.Qmd \
            ../scripts/03_dag.html \
           fig2_part_values.png \
           fig3_shared_values_dag.png \
           fig4_conclusion_part.png \
           fig5_shared_part.png \
           fig6_sci_part.png
	quarto render paper.Qmd --to html

docx: paper.docx
paper.docx: paper.Qmd \
			../scripts/03_dag.html \
           fig2_part_values.png \
           fig3_shared_values_dag.png \
           fig4_conclusion_part.png \
           fig5_shared_part.png \
           fig6_sci_part.png
	quarto render paper.Qmd --to docx


fig2_part_values.png: ../out/03_part_values.png
	cp $< $@
	
fig3_shared_values_dag.png: ../out/03_shared_values_dag.png
	cp $< $@


fig4_conclusion_part.png: ../out/03_conclusion_part.png
	cp $< $@

fig5_shared_part.png: ../out/03_shared_part.png
	cp $< $@

fig6_sci_part.png: ../out/03_sci_part.png
	cp $< $@

