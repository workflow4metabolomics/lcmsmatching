all: peaks.tsv

# Get a set of spectra
spectra.xml:
	wget -O $@ 'http://massbank.eu/api/services/MassBankAPI/searchPeak?mzs=200&relativeIntensity=100&tolerance=100&instrumentTypes=all&ionMode=Both&maxNumResults=0'

spectra.txt: spectra.xml
	xmllint --shell $< <xpath-spectra-ids.xmllint | grep 'content=' | sed 's/^.*content=//' > $@

peaks.tsv: spectra.txt
	./get-massbank-peaks -i $< -o $@

search-output.tsv: peaks.tsv
	../r-msdb/search-mz -d file --url $< --db-fields mztheo=mz,molid=accession,mode=msmode,molmass=mass,chebi=chebiid,kegg=keggid --db-ms-modes pos=pos,neg=neg -i ../r-msdb/test/mz-input.tsv -m pos -o $@

clean:
	$(RM) spectra.xml spectra.txt peaks.tsv search-output.tsv

.PHONY: all clean
