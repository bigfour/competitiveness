all: jasa2016.pdf

jasa2016.pdf: jasa2016.tex refs.bib
	pdflatex jasa2016
	bibtex jasa2016
	pdflatex jasa2016
	bibtex jasa2016
	pdflatex jasa2016

clean: 
	rm *.idx *.ilg *.ind

jasa2016.tex: jasa2016.Rnw
	Rscript -e "library(knitr); knit('jasa2016.Rnw')"
	

mcmc: R/jagsFunction.R
	Rscript R/jagsFunction.R
