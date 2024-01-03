CARGO = cargo


# Targets
all: build

build:
	$(CARGO) build

release:
	$(CARGO) build --release

run:
	$(CARGO) run --release

run_debug:
	$(CARGO) run 

clean:
	$(CARGO) clean

# Phony targets
.PHONY: all build release run run_debug test doc clean
