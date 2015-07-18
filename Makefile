dsync: Config.hs FSAction.hs FSDiff.hs FSInfo.hs FSTime.hs FSType.hs Main.hs MiscUtils.hs
	ghc -O0 -o dsync *.hs

clean:
	rm -f *.hi *.o dsync

install: dsync
	install dsync /usr/bin

.PHONY: clean install

