make:
	ghc --make -Wall -fwarn-tabs Main.hs

clean:
	rm -f Main Tests *.hi *.o
