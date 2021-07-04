L=10
SHELL = /bin/bash

all:
	./numberlink $$RANDOM $(L) | lualatex
	mv jlreq.pdf result.pdf
	rm *.aux *.log

bin:
	cabal install --installdir=. --overwrite-policy=always
