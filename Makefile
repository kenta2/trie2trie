.PHONY: default
default:
	ghc -Wall tree-modify.hs

.PHONY:o
o:
	ghc -Wall -O2 tree-modify.hs

.PHONY:clean
clean:
	rm -f *.o *.hi tree-modify
