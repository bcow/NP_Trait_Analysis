mvtraits := $(wildcard mvtraits/*) $(wildcard mvtraits/**/*)
#npft := 35
#pfts := $(shell {1..$(npft)})
#uni := uni $(shell printf 'uni_%02d' $(pfts))
#multi := multi $(shell printf 'multi_%02d' $(pfts))
#hier := hier
#outputdir := output.n1
#uniFiles := 

.PHONY: all install test figures

all: install test

############################################################
# Install package
############################################################
install: .last.install

.last.install: $(mvtraits)
	Rscript -e "devtools::document('mvtraits')"
	R CMD INSTALL mvtraits
	date > .last.install

check: install
	R CMD check mvtraits

############################################################
# Process TRY data
############################################################

# ...

############################################################
# Test one model
############################################################
testthat: 
	Rscript -e "devtools::test('mvtraits')"

test: .last.test

.last.test: .last.install
	Rscript 01.run.model.R uni_01
	date > .last.test

############################################################
# Run all models
############################################################

run_all: run_uni run_multi run_hier

run_uni: install
	Rscript 02.submit.all.R uni `printf 'uni_%02d ' {1..35}`

run_multi: install
	Rscript 02.submit.all.R multi `printf 'multi_%02d ' {1..35}`

run_hier: install
	Rscript 02.submit.all.R hier

############################################################
# Process outputs
############################################################

outputs: install
	Rscript 03.load.outputs.R

############################################################
# Generate figures 
############################################################

figures: figures/cor.global.hierarchical.png\
    figures/cor.global.multi.png \
    figures/pft.cor.anova.png \
    figures/pft.cor.anova.scaled.png \
    figures/pft.cor.plot.png \
    figures/pft.corrs.gif \
    figures/pft.cov.plot.png \
    figures/stacked.cor.biome.png \
    figures/tot.var.table.txt \
    processed_output

figures/cor.global.hierarchical.png figures/cor.global.multi.png figures/pft.cor.anova.png figures/pft.cor.anova.scaled.png figures/pft.cor.plot.png figures/pft.corrs.gif figures/pft.cov.plot.png figures/stacked.cor.biome.png figures/tot.var.table.txt :  processed_output/*

figures/cor.global.hierarchical.png figures/cor.global.multi.png : results.hier_vs_multi.covar.R
	Rscript results.hier_vs_multi.covar.R

figures/pft.cor.anova.png figures/pft.cor.anova.scaled.png figures/tot.var.table.txt: results.anova.R
	Rscript results.anova.R

figures/pft.corrs.gif : results.pftcorrgif.R
	Rscript results.pftcorrgif.R

figures/stacked.cor.biome.png figures/pft.cov.plot.png figures/pft.cor.plot.png: results.pft.covcor.R
	Rscript results.pft.covcor.R


clean:
	rm -rf mvtraits.Rcheck 

