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

# -------------------------------------------------------------------------

build:
	jbuilder build

install:
	jbuilder install

uninstall:
	jbuilder uninstall

# --------------------------------------------------------------------------
# --- Documentation
# --------------------------------------------------------------------------

PKG=qed
NAME=Qed
JOBS?= -j 4
DEPENDS=
FLAGS=  -use-ocamlfind $(JOBS) \
	-cflags -w,PSUZL+7,-warn-error,PSUZL+7 \
	-cflags -for-pack,$(NAME)

doc: src/$(PKG).odocl
	@echo "Generating '$(NAME)' documentation"
	@ocamlbuild $(DEPENDS) $(FLAGS) \
		-build-dir _doc 	\
		-docflag -t -docflag "$(NAME) Library" \
		-docflag -short-functors \
		src/$(PKG).docdir/index.html
	@cp -f licenses/ceatech.css _doc/src/$(PKG).docdir/style.css
	@echo "Documentation at $(PWD)/_doc/src/qed.docdir/index.html"

clean:
	@echo "Cleaning"
	@jbuilder clean
	@rm -fr _doc

headers:
	headache -c licenses/header.config -h licenses/HEADER \
		src/*.ml src/*.ml[iyl] Makefile
	ocp-indent -i src/*.ml src/*.mli
	opam lint ./opam
	@grep "^name" opam
	@grep "^version" opam META
