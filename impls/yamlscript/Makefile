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


default:

test: test-0 test-1 test-2

test-%:
	@$(MAKE) --no-print-directory -C $(ROOT) test^yamlscript^step$(@:test-%=%) \
	    DEFERRABLE=$(DEFERRABLE) OPTIONAL=$(OPTIONAL)

clean:
	$(RM) -r ../python/__pycache__/
