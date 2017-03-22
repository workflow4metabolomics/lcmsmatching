all:

test:
	$(MAKE) -C $@

clean:
	$(MAKE) -C $@

.PHONY: all clean test
