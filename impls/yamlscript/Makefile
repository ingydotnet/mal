SHELL := bash

ROOT := $(shell cd ../.. && pwd)


default:

test: test-0

test-%:
	@$(MAKE) --no-print-directory -C $(ROOT) test^yamlscript^step$(@:test-%=%)

clean:
	$(RM) -r ../python/__pycache__/
