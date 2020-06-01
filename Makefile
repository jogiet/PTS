.PHONY: doc

OCBFLAGS = -I .
OCB = ocamlbuild -use-menhir -use-ocamlfind $(OCBFLAGS)
LTXMK = latexmk -xelatex -file-line-error --interaction=nonstopmode 

main:
	$(OCB) main.native
	mv main.native pts.exe

DIST_FILES = -I _build/

doc: main
	ocamldoc *.ml -html -charset utf8 -d doc $(DIST_FILES)

%.pdf: %.tex
	$(LTXMK) $<

clean:
	rm -rf _build/ *.exe doc/*.html doc/*.pdf *~ tests/*~

realclean: clean
	latexmk -c tests/*.tex
	rm -f *.pdf tests/*.tex
