LIB_NAME=raft

LIB_FILES+=raft_helper
LIB_FILES+=raft_logic
LIB_FILES+=raft_types
LIB_FILES+=raft_log

LIB_DEPS=

test: 
	$(OCB) test.native
	export OCAMLRUNPARAM="b" && ./test.native 

doc-gen:
	$(OCB) src/$(LIB_NAME).docdir/index.html

## Generic library makefile ## 
include Makefile.opamlib

