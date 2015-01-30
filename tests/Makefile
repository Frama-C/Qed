all: imap 

# -----------------------------


imap:
	make -C .. lib
	@ocamlopt -annot -o intmapref -I ../bin str.cmxa nums.cmxa unix.cmxa ../bin/Qed.cmx -annot Intmapref.ml
	@./intmapref

# -----------------------------

TESTS= abs list pretty

RESULTS=$(addsuffix .result,$(TESTS))
ORACLES=$(addsuffix .oracle,$(TESTS))

# -----------------------------

# run:
# 	make -C .. qed
# 	@echo "--------------------------------------"
# 	@FAILED=""; \
#         for i in $(TESTS) ; \
# 	do \
# 		echo "---- Test $$i" ;\
# 		echo "--------------------------------------" ;\
# 		../bin/qed $$i.qed | tee $$i.result ;\
# 		echo "--------------------------------------" ;\
# 		echo "---- Verify $$i" ;\
# 		echo "--------------------------------------" ;\
# 		diff -uwBN $$i.oracle $$i.result ;\
# 	done ; \
# 	echo "---- Done."; \
# 	echo "--------------------------------------"; \

# update:
# 	@for i in $(TESTS) ; \
# 	do cp $$i.result $$i.oracle ; \
# 	done ;
