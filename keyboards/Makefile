.PHONY: help moonlander/build moonlander/flash moonlander/clean moonlander/update
.PHONY: eyelash_corne/build eyelash_corne/flash
.PHONY: draw moonlander/draw eyelash_corne/draw
.PHONY: all clean

# Default target
help:
	@echo "Keyboard Build Targets:"
	@echo ""
	@echo "Moonlander (QMK):"
	@echo "  moonlander/build     - Build Moonlander firmware"
	@echo "  moonlander/flash     - Build and flash Moonlander firmware"
	@echo "  moonlander/update    - Update QMK submodules"
	@echo "  moonlander/clean     - Clean Moonlander build artifacts"
	@echo ""
	@echo "Eyelash Corne (ZMK):"
	@echo "  eyelash_corne/build  - Build Eyelash Corne firmware (aliases: corne/build)"
	@echo "  eyelash_corne/flash  - Build and flash Eyelash Corne firmware (aliases: corne/flash)"
	@echo ""
	@echo "Keymap Drawing:"
	@echo "  draw                 - Generate keymap SVGs for all keyboards"
	@echo "  moonlander/draw      - Generate keymap SVG for Moonlander"
	@echo "  eyelash_corne/draw   - Generate keymap SVG for Eyelash Corne"
	@echo ""
	@echo "Other:"
	@echo "  all                  - Build all keyboards"
	@echo "  clean                - Clean all build artifacts"

# Moonlander targets
moonlander/build:
	cd moonlander && ./go.sh build

moonlander/flash:
	cd moonlander && ./go.sh flash

moonlander/update:
	cd moonlander && ./go.sh checkout && ./go.sh update

moonlander/clean:
	cd moonlander && ./go.sh clean

# Eyelash Corne targets
eyelash_corne/build:
	cd eyelash_corne && ./go.sh build

eyelash_corne/flash:
	cd eyelash_corne && ./go.sh flash

# Keymap drawing targets
draw:
	./generate-keymaps.sh all

moonlander/draw:
	./generate-keymaps.sh moonlander

eyelash_corne/draw:
	./generate-keymaps.sh eyelash_corne

# Composite targets
all: moonlander/build eyelash_corne/build

clean: moonlander/clean
	rm -rf eyelash_corne/firmwares
