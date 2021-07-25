.PHONY: doc tests

OCBFLAGS = -I .
OCB = ocamlbuild -use-menhir -use-ocamlfind $(OCBFLAGS)
LTXMK = latexmk -xelatex -file-line-error --interaction=nonstopmode

all: main js

main:
	dune build src/main.exe
	ln -sf _build/default/src/main.exe pts.exe

js:
	dune build html/main_js.bc
	js_of_ocaml _build/default/html/main_js.bc
	mv _build/default/html/*.js html/
	cp -r html/. ~/www/PTS/

DIST_FILES = -I _build/default/src/.mylib.objs/byte

doc: main
	ocamldoc src/*.ml -html -charset utf8 -d ./doc $(DIST_FILES)

%.pdf: %.tex
	$(LTXMK) $<

tests:
	./tests/tests.sh

clean:
	rm -rf _build/ *.exe html/*.js doc/*.html doc/*.pdf *~ */*~

realclean: clean
	latexmk -c tests/*.tex
	rm -f *.pdf tests/*.tex
