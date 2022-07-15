PAPER_DIR = paper
TALK_DIR = talk
SCRIPTS_DIR = scripts

all: scripts paper

.PHONY: talk scripts paper install

talk: 
	@echo "build talk/slides"
	cd $(TALK_DIR); $(MAKE)
	
scripts: 
	@echo "scripts"
	cd $(SCRIPTS_DIR); $(MAKE)
	
paper: 
	@echo "paper"
	cd $(PAPER_DIR); $(MAKE)

install:
	@echo "Setting up reproducible environment"
	Rscript -e "renv::restore()"
	Rscript -e "webshot::install_phantomjs()"
