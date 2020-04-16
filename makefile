all: lab19 aTMcomponents

lab19: lab19.ml
	ocamlbuild lab19.byte

aTMcomponents: aTMcomponents.ml
	ocamlbuild aTMcomponents.byte

run:
	./lab19.byte

clean:
	rm -r _build *.byte
