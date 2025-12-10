# Executable name
APP = UltimateTTT

# All Haskell source files
SOURCES = Main.hs CoreGame.hs Solve.hs PrintInOutput.hs

.PHONY: build prof all clean rebuild

# Normal optimized build
build:
	ghc --make -O2 -threaded -rtsopts -with-rtsopts=-N -o $(APP) $(SOURCES)

# Profiling build
prof:
	ghc --make -O2 -prof -fprof-auto -rtsopts -o $(APP) $(SOURCES)

# Build + run tests (if you later add test files)
all: build

# Clean generated files
clean:
	rm -f $(APP) *.o *.hi

# Remove *everything*
rebuild: clean build
