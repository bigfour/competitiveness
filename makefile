all: jasa2016.pdf

jasa2016.pdf: jasa2016.tex refs.bib
	pdflatex jasa2016
	bibtex jasa2016
	pdflatex jasa2016
	bibtex jasa2016
	pdflatex jasa2016

sloan2016.pdf: sloan2016.tex refs.bib
	xelatex sloan2016
	bibtex sloan2016
	xelatex sloan2016
	bibtex sloan2016
	xelatex sloan2016

clean: 
	rm *.idx *.ilg *.ind

jasa2016.tex: jasa2016.Rnw
	Rscript -e "library(knitr); knit('jasa2016.Rnw')"
	
sloan2016.tex: sloan2016.Rnw
	Rscript -e "library(knitr); knit('sloan2016.Rnw')"
	
results: R/MLBscript.R R/NBAscript.R R/NFLscript.R R/NHLscript.R
	Rscript R/MLBscript.R
	Rscript R/NBAscript.R
	Rscript R/NFLscript.R
	Rscript R/NHLscript.R

mline: Rmd/data_wrangling_moneyline.Rmd
	Rscript -e "library(knitr); knit('Rmd/data_wrangling_moneyline.Rmd')"

bigfour: Rmd/data_wrangling_results.Rmd
	Rscript -e "library(knitr); knit('Rmd/data_wrangling_results.Rmd')"

mcmc: R/jagsFunction.R
	Rscript R/jagsFunction.R
	Rscript R/tidy_mcmc.R
