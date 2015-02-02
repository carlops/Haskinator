# Makefile para Haskinator 
all: haskinator

haskinator: 
	ghc --make Haskinator.hs

#SRCS = Haskinator.hs Oraculo.hs 
#HS_PROG = haskinator

#clear_terminal:
#  :! clear

clean:
	rm -f ./*.o ./*.hi
