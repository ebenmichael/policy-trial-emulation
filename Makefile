# Makefile to grab data and run analysis
.PHONY: pdf html all clean

data/covid-19-data : 
	@echo "\nGetting case counts from NYT...\n"
	cd data && git clone git@github.com:nytimes/covid-19-data.git

notebook : data/covid-19-data
	@echo "\nRunning analysis...\n"
	Rscript -e "rmarkdown::render('analysis.Rmd', 'html_notebook')"


clean :
	@echo "\nRemoving notebook files and data...\n"
	rm -rf analysis_cache
	rm -rf analysis_files
	rm -rf data/covid-19-data
	rm analysis.nb.html
