TOOL_XML=lcmsmatching.xml

all:

test:
	bash-testthat/testthat.sh test/test-searchmz

planemo-lint:
	planemo lint --no_xsd $(TOOL_XML)

clean:
	$(MAKE) -C $@

.PHONY: all clean test planemo-lint
