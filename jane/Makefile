
TARGETS=anagram bt ll machine memo pali
# MLS=$(addsuffix .ml,$(TARGETS))
BYTES=$(addsuffix .byte,$(TARGETS))

all: $(BYTES)

%.byte: %.ml
	corebuild -pkg batteries $@
clean:
	-corebuild -clean
