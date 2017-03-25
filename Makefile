all:

test:
	bash-testthat/testthat.sh test/test-searchmz

clean:
	$(MAKE) -C $@

.PHONY: all clean test
