SHELL := bash

ROOT := $(shell cd ../.. && pwd)
BASE := $(shell pwd)

export PERL5LIB := $(BASE)/lib

ifndef BASIC
export DEFERRABLE := 1
export OPTIONAL := 1
endif

ALL_TESTS := $(shell echo test-{0..8})


default:

.PHONY: test
test:
	$(MAKE) test-all BASIC=$(BASIC) |& \
	    grep -E '(^TEST RESULTS|: .* tests$$)'

test-all: $(ALL_TESTS)

$(ALL_TESTS):
	$(MAKE) --no-print-directory -C $(ROOT) test^yamlscript^step$(@:test-%=%) \
	    DEFERRABLE=$(DEFERRABLE) OPTIONAL=$(OPTIONAL)

clean:
	$(RM) -r ../python/__pycache__/
