all: talk site

talk: talk.html
talk.html: talk.qmd
	quarto render talk.qmd

site: ../docs/index.html
../docs/index.html: talk.html
	cp talk.html ../docs/index.html
