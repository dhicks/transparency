PAPER_DIR = paper
TALK_DIR = talk
SCRIPTS_DIR = scripts

all: scripts paper readme

.PHONY: talk scripts paper install readme

talk: 
	@echo "build talk/slides"
	cd $(TALK_DIR); $(MAKE)
	
scripts: 
	@echo "scripts"
	cd $(SCRIPTS_DIR); $(MAKE)
	
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
