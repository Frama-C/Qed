# --------------------------------------------------------------------------
# ---  Qed Toplevel                                                      ---
# --------------------------------------------------------------------------

.PHONY: qed

qed: ../bin/qed

FILES= ground toplevel

# --------------------------------------------------------------------------

CMX=$(FILES:=.cmx)
CMI=$(FILES:=.cmi)

../bin/qed: ../bin/Qed.cmx ../bin/Qed.cmi $(CMX)
	$(OCAMLOPT) -o ../bin/qed $(INCLUDES) nums.cmxa Qed.cmx $(CMX)

../bin/Qed.cmx ../bin/Qed.cmi:
	make -C ../src opt

$(CMX): ../bin/Qed.cmx
$(CMI): ../bin/Qed.cmi

INCLUDES= -I ../bin
include ../src/MakeOcaml
