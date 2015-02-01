all: haskinator

haskinator:  
ghc --make Haskinator.hs

clear_terminal:
  :! clear

clean:
rm -f ./*.o ./*.hi Haskinator
