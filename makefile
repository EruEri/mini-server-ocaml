mini-server: bind.c server.ml http.ml
	ocamlopt -o mini-server bind.c str.cmxa unix.cmxa http.ml server.ml