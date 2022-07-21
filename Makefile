PAPER_DIR = paper
TALK_DIR = talk
SCRIPTS_DIR = scripts

all: scripts paper readme

.PHONY: talk scripts paper install readme

talk: 
	@echo "build talk/slides"
	cd $(TALK_DIR); $(MAKE)
	
# scripts: 
# 	@echo "scripts"
# 	cd $(SCRIPTS_DIR); $(MAKE)
	
paper: 
	@echo "paper"
	cd $(PAPER_DIR); $(MAKE)
	
readme: readme.md
readme.md: readme.Rmd
	Rscript -e "rmarkdown::render('readme.Rmd')"

install:
	@echo "Setting up reproducible environment"
	Rscript -e "renv::restore()"
	Rscript -e "install.packages('Hmisc')"
	Rscript -e "webshot::install_phantomjs()"


R := R
data := data

scripts: $(SCRIPTS_DIR)/03_dag.html

$(SCRIPTS_DIR)/03_dag.html: $(SCRIPTS_DIR)/03_dag.R \
             $(R)/reg_tbl.R \
             $(R)/plot_adjustments.R \
             $(R)/reg_plots.R \
             $(data)/emad.Rds \
             $(data)/data.Rds
	Rscript -e "rmarkdown::render('$(SCRIPTS_DIR)/03_dag.R')"

$(SCRIPTS_DIR)/02_analysis.html: $(SCRIPTS_DIR)/02_analysis.R \
                  $(data)/data.csv
	Rscript -e "rmarkdown::render('$(SCRIPTS_DIR)/02_analysis.R')"
	
$(data)/emad.Rds: $(SCRIPTS_DIR)/01_clean.html
$(data)/data.Rds: $(SCRIPTS_DIR)/01_clean.html
$(SCRIPTS_DIR)/01_clean.html: $(SCRIPTS_DIR)/01_clean.R
	Rscript -e "rmarkdown::render('$(SCRIPTS_DIR)/01_clean.R')"
