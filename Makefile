# --------------------------------------------------------------------------
# ---  QED                                                               ---
# --------------------------------------------------------------------------

.PHONY: help all lib doc pdf qed top clean tests

help:
	@cat README.txt

all:
	make -C src clean depend
	make -C src byte opt doc

lib:
	make -C src depend
	make -C src byte opt

doc:
	make -C src doc

pdf:
	make -C tex pdf

qed:
	make -C src depend
	make -C src opt
	make -C top depend
	make -C top qed

tests:
	make -C src byte opt
	make -C top qed
	make -C tests all

clean:
	make -C src clean
	make -C top clean
	make -C tex clean
	rm -fr html bin
