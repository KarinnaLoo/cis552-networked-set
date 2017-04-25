make:
	ghc --make -Wall Main.hs

clean:
	rm -f Main *.hi *.o
