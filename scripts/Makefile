out := ../out
data := ../data
R := ../R

all: 03_dag.html

03_dag.html: 03_dag.R \
             $(R)/reg_tbl.R \
             $(R)/plot_adjustments.R \
             $(R)/reg_plots.R \
             $(data)/emad.Rds \
             $(data)/data.Rds
	Rscript -e "rmarkdown::render('03_dag.R')"

02_analysis.html: 02_analysis.R \
                  $(data)/data.csv
	Rscript -e "rmarkdown::render('02_analysis.R')"
	
$(data)/emad.Rds: 01_clean.html
$(data)/data.Rds: 01_clean.html
01_clean.html: 01_clean.R \
               $(data)/full_replication_scientists_values.csv \
               $(data)/replication_preference.csv
	Rscript 01_clean.R
