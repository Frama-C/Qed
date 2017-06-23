##########################################################################
#                                                                        #
#  This file is part of Qed Library                                      #
#                                                                        #
#  Copyright (C) 2007-2016                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

.PHONY: all help build install doc

all: build

help:
	@echo "-- Qed"
	@echo ""
	@echo "  make build     compile the library"
	@echo "  make install   install the library"
	@echo "  make uninstall uninstall the library"
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

TARGETS= $(addprefix src/$(PKG).,cmo cmx cmxa cmxs cmi a)

build:
	@echo "Build Qed."
	@ocamlbuild $(DEPENDS) $(FLAGS) $(TARGETS)

# --------------------------------------------------------------------------
# --- Documentation
# --------------------------------------------------------------------------

doc: src/$(PKG).odocl
	@echo "Generating '$(NAME)' documentation"
	@ocamlbuild $(DEPENDS) $(FLAGS) \
		-docflag -t -docflag "$(NAME) Library" \
		-docflag -short-functors \
		src/$(PKG).docdir/index.html
	@cp -f licenses/ceatech.css _build/src/$(PKG).docdir/style.css
	@echo "Documentation at $(PWD)/qed.docdir/index.html"

src/$(PKG).odocl: src/$(PKG).mlpack
	@rm -f $@
	@cp $< $@
	@chmod a-w $@

# --------------------------------------------------------------------------
# ---  Install                                                           ---
# --------------------------------------------------------------------------

install:
	@echo "Install Qed."
	@if [ -e $(shell ocamlfind printconf destdir)/$(PKG) ] ;\
	 then ocamlfind remove $(PKG) ; fi
	@ocamlfind install $(PKG) META $(addprefix _build/,$(TARGETS))

uninstall:
	@echo "Uninstall Qed."
	@ocamlfind remove $(PKG)

# --------------------------------------------------------------------------

clean:
	@echo "Cleaning"
	@rm -f src/$(PKG).odocl src/*~
	@ocamlbuild -clean

headers:
	headache -c licenses/header.config -h licenses/HEADER \
		src/*.ml src/*.ml[iyl] Makefile
	ocp-indent -i src/*.ml src/*.mli
	opam lint ./opam
	@grep "^name" opam
	@grep "^version" opam META

