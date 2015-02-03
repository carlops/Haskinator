# Makefile para Haskinator 
Hask: Haskinator.hs
	ghc --make Haskinator.hs Oraculo.hs -o haskinator

clean:
	rm -f ./*.o ./*.hi haskinator
