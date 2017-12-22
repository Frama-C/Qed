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

doc:
	@echo "Generating '$(NAME)' documentation"
	@rm -fr html
	@mkdir -p html
	@jbuilder build
	@ocamlfind ocamldoc -html -d html -t "$(NAME) Library" \
		  -package zarith \
		  -short-functors -I _build/default/src -open Qed \
		  src/*.mli src/logic.ml src/engine.ml
	@cp -f licenses/ceatech.css html/style.css
	@echo "Documentation at html/index.html"

clean:
	@echo "Cleaning"
	@jbuilder clean
	@rm -fr html

lint:
	headache -c licenses/header.config -h licenses/HEADER \
		src/*.ml src/*.ml[iyl] Makefile
	ocp-indent -i src/*.ml src/*.mli
