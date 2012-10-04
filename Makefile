xbattbar: main.hs XBattBar.hs Backend.hs Types.hs
	ghc --make -with-rtsopts=-V0 main.hs -o $@

all: xbattbar
