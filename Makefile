GHC = ghc 
BUILDDIR = build 
GHCFLAGS = -isrc -O2 -W -odir $(BUILDDIR) -hidir $(BUILDDIR)

SOURCE = $(shell find src/ -name "*.hs")

all: simple

simple: $(SOURCE)
	$(GHC) $(GHCFLAGS) src/Main.hs -o simple  

clean: 
	rm -f build/* simple 
