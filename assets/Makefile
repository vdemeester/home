RH_VPN := $(shell command -v redhat-vpn 2> /dev/null)

.PHONY: vpn
vpn: $(RH_VPN)
	redhat-vpn

.PHONY: build
build: ./src/home/Makefile
	make -C ./src/home build

.PHONY: update
update: ./src/home/Makefile
	make -C ./src/home switch

.PHONY: pull
pull: ./src/home/Makefile
	make -C ./src/home pull

.PHONY: upgrade
upgrade: pull update
