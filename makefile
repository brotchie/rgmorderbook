GHC=ghc
GHCFLAGS=-O2

Main : Main.hs RGM.hs
	$(GHC) $(GHCFLAGS) --make Main

run : Main
	./Main

benchmark : Main
	time ./Main > /dev/null

clean : 
	rm -f Main *.o *.hi
