.PHONY: doc

LTXMK = latexmk -xelatex -file-line-error --interaction=nonstopmode 

main:
	dune build src/main.exe
	ln -sf _build/default/src/main.exe pts.exe


doc: main
	dune build @doc-private

%.pdf: %.tex
	$(LTXMK) $<

clean:
	rm -rf _build/ *.exe *~ tests/*~

realclean: clean
	latexmk -c tests/*.tex
	rm -f *.pdf tests/*.tex
