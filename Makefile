GHC = ghc 
BUILDDIR = build 
GHCFLAGS = -O2 -W 
GHCBUILDFLAGS = -isrc -odir $(BUILDDIR) -hidir $(BUILDDIR)

SOURCE = $(shell find src/ -name "*.hs")

EXECUTABLE = ibc0 

all: $(EXECUTABLE)

$(EXECUTABLE): $(SOURCE)
	$(GHC) $(GHCFLAGS) $(GHCBUILDFLAGS) src/Main.hs -o $(EXECUTABLE)

clean: 
	rm -f build/* $(EXECUTABLE) examples/*.bc0
