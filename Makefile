.PHONY: all help build install doc

all: build

help:
	@echo "-- Qed"
	@echo ""
	@echo "  make build     compile the library"
	@echo "  make install   install the library"
	@echo "  make doc       generate documentation"
	@echo "  make clean     remove generated files"
	@echo "  make headers   normalize files"

# --------------------------------------------------------------------------
# ---  Build                                                             ---
# --------------------------------------------------------------------------

PKG=qed
NAME=Qed
JOBS?= -j 4
DEPENDS=
FLAGS=  -use-ocamlfind $(JOBS) \
	-cflags -w,PSUZL+7,-warn-error,PSUZL+7 \
	-cflags -for-pack,$(NAME)

TARGETS= src/$(PKG).cmo src/$(PKG).cmx

build:
	@echo "Build Qed."
	@ocamlbuild $(DEPENDS) $(FLAGS) $(TARGETS)

# --------------------------------------------------------------------------

install:
	@echo "Install Qed."

doc:
	@echo "Qed Documentation."

clean:
	@echo "Cleaning"
	@ocamlbuild -clean

headers:
	headache -c licences/header.config -h licences/HEADER \
		src/*.ml src/*.ml[iyl] Makefile
	ocp-indent -i src/*.ml src/*.mli

