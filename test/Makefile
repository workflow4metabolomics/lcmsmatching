export BIODB_CACHE_DIRECTORY=$(CURDIR)/../lcms.biodb.cache

all: clean
	./test-list-chrom-cols
	../bash-testthat/testthat.sh test-lcmsmatching

clean:
	$(RM) *-output.tsv
	$(RM) *-output-peaks.tsv
	$(RM) *.html *-main.tsv *-peak.tsv *.txt *.log

.PHONY: all clean
