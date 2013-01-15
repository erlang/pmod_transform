.PHONY: all
all: src tests

.PHONY: src
src:
	$(MAKE) -C src

.PHONY: test
tests: src
	$(MAKE) -C tests
