OCB_INC   = -I src -I tests
OCB_FLAGS = -use-ocamlfind -pkgs ocaml-protoc
OCB       = ocamlbuild $(OCB_FLAGS) $(OCB_INC)

.PHONY: test gen lib.native lib.byte lib.install lib.uninstall clean doc 

test: 
	$(OCB) test.native
	export OCAMLRUNPARAM="b" && ./test.native 

gen:
	ocaml-protoc -ml_out src src/raft.proto

lib.native:
	$(OCB) raft.cmxa
	$(OCB) raft.cmxs

lib.byte:
	$(OCB) raft.cma

LIB_FILES+=raft_pb 
LIB_FILES+=raft_helper
LIB_FILES+=raft_logic
LIB_FILES+=raft_types
LIB_FILES+=raft_role
LIB_FILES+=raft_log

LIB_BUILD     =_build/src/
LIB_INSTALL   = META 
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.mli,$(LIB_FILES))
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmi,$(LIB_FILES))
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.annot,$(LIB_FILES))
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmo,$(LIB_FILES))
LIB_INSTALL  +=$(LIB_BUILD)/raft.cma 

LIB_INSTALL  +=-optional  
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmx,$(LIB_FILES))
LIB_INSTALL  +=$(patsubst %,$(LIB_BUILD)/%.cmt,$(LIB_FILES))
LIB_INSTALL  +=$(LIB_BUILD)/raft.cmxa 
LIB_INSTALL  +=$(LIB_BUILD)/raft.cmxs
LIB_INSTALL  +=$(LIB_BUILD)/raft.a

lib.install:
	ocamlfind install raft $(LIB_INSTALL)

lib.uninstall:
	ocamlfind remove raft

clean:
	$(OCB) -clean

doc:
	$(OCB) src/raft.docdir/index.html
