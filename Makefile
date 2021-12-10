.PHONY: test check

.PHONY: server check

.PHONY: client check


build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop util

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

server:
	OCAMLRUNPARAM=b dune exec server/server.exe

client:
	OCAMLRUNPARAM=b dune exec client/client.exe

doc:
	dune build @doc


