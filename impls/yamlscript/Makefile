SHELL := bash

ROOT := $(shell cd ../.. && pwd)
BASE := $(shell pwd)

export PERL5LIB := $(BASE)/lib

ifndef BASIC
export DEFERRABLE := 1
export OPTIONAL := 1
endif

IMPL ?= yamlscript

ALL_TESTS := $(shell echo test-{{0..9},A})


default:

.PHONY: test
test:
	$(MAKE) test-all BASIC=$(BASIC) |& \
	    grep -E '(^TEST RESULTS|: .* tests$$)'

test-ys:
	../perl.2/repl eg/99-bottles.mal 3 >/dev/null
	../perl.2/repl eg/99-bottles-0.ys 3 >/dev/null
	../perl.2/repl eg/99-bottles-1.ys 3 >/dev/null
	:
	../perl.2/repl lib/ys.ys eg/99-bottles.mal 3 >/dev/null
	../perl.2/repl lib/ys.ys eg/99-bottles-0.ys 3 >/dev/null
	../perl.2/repl lib/ys.ys eg/99-bottles-1.ys 3 >/dev/null

test-all: $(ALL_TESTS)

$(ALL_TESTS):
	$(MAKE) --no-print-directory -C $(ROOT) test^$(IMPL)^step$(@:test-%=%) \
	    DEFERRABLE=$(DEFERRABLE) OPTIONAL=$(OPTIONAL)

clean:
	$(RM) -r ../python/__pycache__/
