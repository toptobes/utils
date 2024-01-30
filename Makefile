.PHONY: ut

LOC?=/usr/local/bin

ut:
	@hpack
	@cabal build app -j --ghc-options=-j
	@sudo cp $(shell cabal list-bin app) $(LOC)/ut
	@echo "------------"
	@ls $(LOC)
