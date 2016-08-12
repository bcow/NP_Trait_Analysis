mvtraits := $(wildcard mvtraits/**/*)
#npft := 35
#pfts := $(shell {1..$(npft)})
#uni := uni $(shell printf 'uni_%02d' $(pfts))
#multi := multi $(shell printf 'multi_%02d' $(pfts))
#hier := hier
#outputdir := output.n1
#uniFiles := 

.PHONY: all install test

all: install test

############################################################
# Install package
############################################################
install: .last.install

.last.install: $(mvtraits)
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
test: .last.test

.last.test:
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



clean:
	rm -rf mvtraits.Rcheck 

