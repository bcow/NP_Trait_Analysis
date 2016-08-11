mvtraits=$(wildcard mvtraits/**/*)

all: install test

install: $(mvtraits)
	R CMD INSTALL mvtraits

check: install
	R CMD check mvtraits

test: install
	Rscript 01.run.model.R uni_01

clean:
	rm -rf mvtraits.Rcheck 

