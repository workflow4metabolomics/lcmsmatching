test.match.mz.no.y.col.in.output <- function() {
	# Test that no 'y' column is added in output when running an MZ match
	call.search.mz(c('-m pos', '-i', 'mz-input-small.tsv', '-o', 'mz-output.tsv', '--same-rows'))
	df <- read.table(get.res.path('mz-output.tsv'), header = TRUE)
	checkTrue( ! 'y' %in% colnames(df))
}

# 2017-01-18 Failure encountered on Galaxy production instance (lcmsmatching version 2.1.3) with Florence Castelli
test.2017.01.18.florence.castelli <- function() {

	res.name <- '2017-01-18-florence-castelli'
	res.dir <- file.path(dirname(script.path), 'res', res.name)
	main.output <- file.path(dirname(script.path), paste(res.name, 'main.tsv', sep = '-'))
	peak.output <- file.path(dirname(script.path), paste(res.name, 'peak.tsv', sep = '-'))
	html.output <- file.path(dirname(script.path), paste(res.name, 'html', sep = '.'))

	call.search.mz(c('-d', 'file',
	                 '--url', file.path(res.dir, 'db.tsv'),
	                 '--db-fields', "mztheo=mztheo,chromcolrt=colrt,compoundid=molid,chromcol=col,msmode=mode,peakattr=attr,peakcomp=comp,fullnames=molnames,compoundmass=molmass,compoundcomp=molcomp,inchi=inchi,inchikey=inchikey,pubchemcompid=pubchem,chebiid=chebi,hmdbid=hmdb,keggid=kegg",
					 '--db-ms-modes', 'pos=POS,neg=NEG',
	                 '-i', file.path(res.dir, 'input.tsv'),
	                 '--input-col-names', 'mz=mz,rt=rt',
	                 '-m', 'neg',
	                 '-p', '10.0',
	                 '-s', '0.0',
	                 '-c', "zicphilic-150*5*2.1-42min-shimadzuexactive",
	                 '-x', '5.0',
	                 '-y', '0.8',
	                 '--precursor-match',
	                 '--precursor-rt-tol', '5.0',
	                 '--pos-prec', "'[(M+H)]+,[M+H]+,[(M+Na)]+,[M+Na]+,[(M+K)]+,[M+K]+'",
					 '--neg-prec', "'[(M-H)]-,[M-H]-,[(M+Cl)]-,[M+Cl]-'",
					 '-o', main.output,
					 '--peak-output-file', peak.output,
					 '--html-output-file', html.output
					 ), use.global.conn.flags = FALSE)

	# TODO Should be testing output files here.
}

# Test bug replacing values of columns by integers
test.2017.01.26.w4m.sacurine.phenomenal.demo <- function() {

	res.name <- '2017-01-26-w4m-sacurine-phenomenal-demo'
	res.dir <- file.path(dirname(script.path), 'res', res.name)
	input <- file.path(res.dir, 'Biosigner_variableMetadata.tsv')
	main.ref.output <- file.path(res.dir, 'main.tsv')
	peak.ref.output <- file.path(res.dir, 'peak.tsv')
	main.output <- file.path(dirname(script.path), paste(res.name, 'main.tsv', sep = '-'))
	peak.output <- file.path(dirname(script.path), paste(res.name, 'peak.tsv', sep = '-'))
	html.output <- file.path(dirname(script.path), paste(res.name, 'html', sep = '.'))

	call.search.mz(c('-d', 'file',
					 '--url', file.path(res.dir, 'massbank-neg-ms1-peaks.tsv'),
					 '--db-fields', 'mztheo=peak.mz,chromcolrt=chromcolrt,compoundid=accession,chromcol=chromcol,msmode=msmode,peakcomp=peak.formula,fullnames=name,compoundmass=mass,compoundcomp=formula,inchi=inchi,inchikey=inchikey,pubchemcompid=pubchemcompid,chebiid=chebiid,hmdbid=hmdbid,keggid=keggid',
					 '--db-ms-modes', 'pos=pos,neg=neg',
					 '-i', input,
					 '--input-col-names', 'mz=mass_to_charge,rt=retention_time',
					 '-m', 'neg',
					 '-p', '10.0',
					 '-s', '0.0',
					 '--same-rows',
					 '--same-cols',
					 '-o', main.output,
					 '--peak-output-file', peak.output,
					 '--html-output-file', html.output
					 ), use.global.conn.flags = FALSE)

	# Load files
	input.df <- read.table(input, sep = "\t", header = TRUE)
	main.df <- read.table(main.output, sep = "\t", header = TRUE)
	main.ref.df <- read.table(file.path(main.ref.output), sep = "\t", header = TRUE)
	peak.df <- read.table(peak.output, sep = "\t", header = TRUE)
	peak.ref.df <- read.table(file.path(peak.ref.output), sep = "\t", header = TRUE)

	# Test order of columns in output: we should get the same columns as input file first, then the new columns
	input.cols <- colnames(input.df)
	checkIdentical(colnames(main.df)[1:length(input.cols)], input.cols, "Input columns should be at beginning of main output data frame.")
	checkIdentical(colnames(peak.df)[1:length(input.cols)], input.cols, "Input columns should be at beginning of peak output data frame.")

	# Check that outputs are identical
	checkIdentical(main.df, main.ref.df, "Main output does not match reference output.")
	checkIdentical(peak.df, peak.ref.df, "Peak output does not match reference output.")
}
