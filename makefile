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

CXXFLAGS=-std=gnu++0x -O3
LDLIBS=-lrt
fastreader: reader.cpp fastreader.cpp reader.h
