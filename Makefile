build:
	ocamlbuild -Is src/ lis.native

run:
	./lis.native

all:
	 ocamlbuild -Is src/ lis.native && ./lis.native
