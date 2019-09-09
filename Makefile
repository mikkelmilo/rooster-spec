build:
	rm -f theories/*.vo ; dune clean ; dune build --profile release

install:
	rm -f theories/*.vo ; dune clean ; dune build --profile release ; dune install

clean:
	rm -f theories/*.vo ;
	rm *.hs
	rm *.smt2
	rm *.ml
	rm *.mli