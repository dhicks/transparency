PAPER_DIR = paper
TALK_DIR = talk
SCRIPTS_WD = scripts

all: pipe paper

.PHONY: install
install:
	@echo "Setting up reproducible environment"
	Rscript -e "renv::restore()"
	Rscript -e "webshot::install_phantomjs()"


paper: 
	@echo "paper"
	cd $(PAPER_DIR); $(MAKE)

talk: 
	@echo "build talk/slides"
	cd $(TALK_DIR); $(MAKE)
	
pipe: 
	@echo "pipe"
	cd $(SCRIPTS_WD); $(MAKE)
	
