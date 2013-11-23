
TARG=gr1.native gr2.native
all: $(TARG)

gr1.native: gr1.ml
	corebuild -libs graphics $@

gr2.native: gr2.ml gr1.native
	corebuild -libs graphics $@

clean:
	-corebuild -clean
