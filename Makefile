OCB_INC   = -I src -I tests
OCB_FLAGS = -use-ocamlfind -pkgs ocaml-protoc
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

.PHONY: all gen clean 

all:
	$(OCB) test.native

gen:
	ocaml-protoc -ml_out src src/raft.proto

clean:
	$(OCB) -clean
