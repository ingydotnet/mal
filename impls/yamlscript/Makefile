SHELL := bash

ROOT := $(shell cd ../.. && pwd)
BASE := $(shell pwd)

export PERL5LIB := $(BASE)/lib

DEFERRABLE ?=
OPTIONAL ?=
ifdef ALL
export DEFERRABLE := 1
export OPTIONAL := 1
endif

ALL_TESTS := $(shell echo test-{0..6})


default:

.PHONY: test
test:
	$(MAKE) test-all ALL=$(ALL) |& \
	    grep -E '(^TEST RESULTS|: .* tests$$)'

test-all: $(ALL_TESTS)

$(ALL_TESTS):
	$(MAKE) --no-print-directory -C $(ROOT) test^yamlscript^step$(@:test-%=%) \
	    DEFERRABLE=$(DEFERRABLE) OPTIONAL=$(OPTIONAL)

clean:
	$(RM) -r ../python/__pycache__/
