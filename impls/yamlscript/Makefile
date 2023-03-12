SHELL := bash

ROOT := $(shell cd ../.. && pwd)
BASE := $(shell pwd)

export PERL5LIB := $(BASE)/lib

ifndef BASIC
export DEFERRABLE := 1
export OPTIONAL := 1
endif

impl ?= perl.2
IMPL ?= $(shell basename $(BASE))

ALL_TESTS := $(shell echo test-{{0..9},A})

GREP_RESULTS := grep -E '(^TEST RESULTS|: .* tests$$)'

GOT_STDOUT := grep -q .


default:

.PHONY: test
test:
	time $(MAKE) test-all BASIC=$(BASIC) |& $(GREP_RESULTS)
	echo

test-expr:
	prove -l -v test/

test-all: $(ALL_TESTS)

test-repl:
	./repl -e '(prn :ok)' | $(GOT_STDOUT)
	:
	./repl eg/99-bottles.mal 3 | $(GOT_STDOUT)
	./repl eg/99-bottles.mal 3 --ys | $(GOT_STDOUT)
	:
	./repl eg/99-bottles-0.ys 3 | $(GOT_STDOUT)
	./repl eg/99-bottles-0.ys 3 --ys | $(GOT_STDOUT)
	:
	./repl eg/99-bottles-1.ys 3 | $(GOT_STDOUT)
	./repl eg/99-bottles-1.ys 3 --ys | $(GOT_STDOUT)
	:
	./repl eg/99-bottles-2.ys 3 | $(GOT_STDOUT)
	./repl eg/99-bottles-2.ys 3 --ys | $(GOT_STDOUT)
	:
	./repl eg/99-bottles-3.ys 3 | $(GOT_STDOUT)
	./repl eg/99-bottles-3.ys 3 --ys | $(GOT_STDOUT)
	:
	./repl eg/99-bottles-4.ys 3 | $(GOT_STDOUT)
	./repl eg/99-bottles-4.ys 3 --ys | $(GOT_STDOUT)
	./repl eg/99-bottles-4.ys --ys | $(GOT_STDOUT)

test-p2:
	$(MAKE) -C ../$(impl) test
	echo

test-ok: test-expr test test-p2 test-repl

test-docker: docker-build
	$(MAKE) -C $(ROOT) DOCKERIZE=1 test^$(IMPL)

test-self-host: docker-build
	time YS_FAST=1 $(MAKE) -C $(ROOT) MAL_IMPL=$(IMPL) test^mal^step{{0..4},{6..9},A} |& \
	    $(GREP_RESULTS)

$(ALL_TESTS):
	time $(MAKE) --no-print-directory -C $(ROOT) test^$(IMPL)^step$(@:test-%=%) \
	    DEFERRABLE=$(DEFERRABLE) OPTIONAL=$(OPTIONAL) || [[ $@ == test-6 ]]

docker-build:
	$(MAKE) -C $(ROOT) docker-build^$(IMPL)
	@echo
