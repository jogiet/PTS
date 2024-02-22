.PHONY: doc tests

LTXMK = latexmk -xelatex -file-line-error --interaction=nonstopmode

all: main js

main:
	dune build @@src/all

js:
	dune build @@html/all

DIST_FILES = -I _build/default/src/.mylib.objs/byte

doc: main
	ocamldoc src/*.ml -html -charset utf8 -d ./doc $(DIST_FILES)

%.pdf: %.tex
	$(LTXMK) $<

tests:
	./tests/tests.sh

clean:
	dune clean
	rm -rf doc/*.html doc/*.pdf *~ */*~

realclean: clean
	latexmk -c tests/*.tex
	rm -f *.pdf tests/*.tex
