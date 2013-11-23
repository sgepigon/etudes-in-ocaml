
TARG=liblist.byte
all: $(TARG) run

%.byte: %.ml
	corebuild $@

run: $(TARG)

clean:
	-corebuild -clean
