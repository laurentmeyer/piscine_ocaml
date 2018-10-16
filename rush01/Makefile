RESULT = tama
SOURCES =	Points.ml Vitals.ml Actions.ml Parse.ml 		\
			Tama.ml Display.ml Events.ml main.ml
LIBS = bigarray sdl sdlloader sdlttf
INCDIRS = +sdl
PACKS = extlib
OCAMLLDFLAGS = -cclib "-framework Cocoa"
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)

all: .install

.install:
	printf "installing env"
	brew install sdl2
	brew install sdl2_ttf
	brew install opam
	opam install extlib
	opam instal ocamlsdl
	touch .install