all: lab19

lab19: lab19.ml
	ocamlbuild lab19.byte

run:
	./lab19.byte

clean:
	rm -r _build *.byte
