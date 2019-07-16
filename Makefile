GHC = ghc 
BUILDDIR = build 
GHCFLAGS = -O2 -W -odir $(BUILDDIR) -hidir $(BUILDDIR)

SOURCE = $(shell find . -name "*.hs")

all: simple

simple: $(SOURCE)
	$(GHC) $(GHCFLAGS) Main.hs -o simple  

clean: 
	rm -f build/* simple 
