CARGO ?= cargo
INSTALL_DIR := ../../home/bin

.PHONY: build install clean

build:
	$(CARGO) build --release

install: build
	cp target/release/tmux-sessionizer-scan $(INSTALL_DIR)/

clean:
	$(CARGO) clean
