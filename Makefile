build:
	rm -f theories/*.vo ; dune clean ; dune build --profile release

install:
	rm -f theories/*.vo ; dune clean ; dune build --profile release ; dune install

clean:
	rm -f theories/*.vo ;
	rm -f *.hs
	rm -f *.smt2