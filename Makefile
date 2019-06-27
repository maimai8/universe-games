SOURCES = game.ml
PACKS = universeJs
RESULT = game
OCAMLMAKEFILE = /home/isstaff/asai/include/OCamlMakefile
include $(OCAMLMAKEFILE)

$(RESULT).js : byte-code
	js_of_ocaml $(RESULT)
