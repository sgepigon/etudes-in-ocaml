
CVER=hmm
OVER=hmm.byte

all: $(CVER) $(OVER) run

$(CVER): hmm.c
	gcc -std=c99 hmm.c -o $@

$(OVER): hmm.ml
	corebuild $@

.PHONY: run
run: $(CVER) $(OVER)
	./$(CVER)
	./$(OVER)

clean:
	-corebuild -clean
	-rm -f hmm
