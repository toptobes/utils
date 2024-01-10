.PHONY: ut

ut:
	@hpack
	@cabal build app
	@sudo cp $(shell cabal list-bin app) /usr/local/bin/ut
	@echo "------------"
	@ls /usr/local/bin
