test.match.mzrt.2cols.prec <- function() {
	# Match on mz/rt values with two columns and precusor match, and check that there are no duplicated lines
	call.search.mz(c('-m pos', '-c uplc-c8,uplc-c18', '--rttolx 5', '--rttoly 0.8', '--precursor-match', '-i', 'mzrt-input-small.tsv', '-o', 'mzrt-output.tsv'))
	df <- read.table(get.res.path('mzrt-output.tsv'), header = TRUE)
	checkTrue( ! any(duplicated(df)))
}

zlong.test.match.mzrt.2cols.prec <- function() {
	# Match on mz/rt values with two columns and precusor match, and check that there are no duplicated lines
	call.search.mz(c('-m pos', '-c uplc-c8,uplc-c18', '--rttolx 5', '--rttoly 0.8', '--precursor-match', '-i', 'mzrt-input.tsv', '-o', 'mzrt-output.tsv'))
	df <- read.table(get.res.path('mzrt-output.tsv'), header = TRUE)
	checkTrue( ! any(duplicated(df)))
}

test.precursors.lists <- function() {
	call.search.mz(c('-m pos', '--precursor-match', '-i', 'mzrt-input-small.tsv', '-o', 'mzrt-output.tsv', '--pos-prec "[(M+H)]+,[M+H]+,[(M+Na)]+,[M+Na]+,[(M+K)]+,[M+K]+"', '--neg-prec "[(M-H)]-,[M-H]-,[(M+Cl)]-,[M+Cl]-"'))
	call.search.mz(c('-m pos', '-c uplc-c8,uplc-c18', '--rttolx 5', '--rttoly 0.8', '--precursor-match', '-i', 'mzrt-input-small.tsv', '-o', 'mzrt-output.tsv', '--pos-prec "[(M+H)]+,[M+H]+,[(M+Na)]+,[M+Na]+,[(M+K)]+,[M+K]+"', '--neg-prec "[(M-H)]-,[M-H]-,[(M+Cl)]-,[M+Cl]-"'))
}

test.same.cols <- function() {
	x <- read.table(get.res.path('mzrt-input-small-morecols.tsv'), header = TRUE)
	extra.cols <- colnames(x)[ ! colnames(x) %in% c('mz', 'rt')]
	call.search.mz(c('-m pos', '-c uplc-c8,uplc-c18', '--rttolx 5', '--rttoly 0.8', '-i', 'mzrt-input-small-morecols.tsv', '-o', 'mzrt-output.tsv'))
	df <- read.table(get.res.path('mzrt-output.tsv'), header = TRUE)
	checkTrue( ! any(extra.cols %in% colnames(df)))
	call.search.mz(c('-m pos', '-c uplc-c8,uplc-c18', '--rttolx 5', '--rttoly 0.8', '-i', 'mzrt-input-small-morecols.tsv', '-o', 'mzrt-output.tsv', '--same-cols'))
	df <- read.table(get.res.path('mzrt-output.tsv'), header = TRUE)
	checkTrue(all(extra.cols %in% colnames(df)))
}

test.same.rows <- function() {
	call.search.mz(c('-m pos', '-c uplc-c8,uplc-c18', '--rttolx 5', '--rttoly 0.8', '-i', 'mzrt-input-small.tsv', '-o', 'mzrt-output.tsv', '--same-rows'))
	x <- read.table(get.res.path('mzrt-input-small.tsv'), header = TRUE)
	y <- read.table(get.res.path('mzrt-output.tsv'), header = TRUE)
	checkTrue(nrow(x) == nrow(y))
}
	
test.same.rows.with.peaks.output <- function() {
	# Test when running an mz/rt search with --same-rows option, we get the same number of rows in output
	call.search.mz(c('-m pos', '-i', 'mzrt-input-small.tsv', '-o', 'mzrt-output.tsv', '--same-rows', '--all-cols', '-x 5.0', '-y 0.8', '--peak-output-file', 'mzrt-output-peaks.tsv'))
	dfin <- read.table(get.res.path('mzrt-input-small.tsv'), header = TRUE)
	dfout <- read.table(get.res.path('mzrt-output.tsv'), header = TRUE)
	checkTrue(nrow(dfin) == nrow(dfout))
}

zlong.test.peak.output.file <- function() {
	peak.output.file <- 'mzrt-output-peaks.tsv'
	output.file <- 'mzrt-output.tsv'
	call.search.mz(c('-m pos', '-c uplc-c8,uplc-c18', '--rttolx 5', '--rttoly 0.8', '-i', 'mzrt-input.tsv', '-o', output.file, '--same-rows', '--peak-output-file', peak.output.file))
	x <- read.table(get.res.path('mzrt-input.tsv'), header = TRUE)
	y <- read.table(get.res.path('mzrt-output.tsv'), header = TRUE)
	z <- read.table(get.res.path('mzrt-output-peaks.tsv'), header = TRUE)
	x.out <- read.table(output.file, header = TRUE)
	x.out.peak <- read.table(peak.output.file, header = TRUE)
	checkTrue(nrow(x) == nrow(y))
	checkTrue(all(colnames(x) %in% colnames(z)))
	checkTrue(all(colnames(x.out) %in% colnames(z)))
	checkTrue(all(c(MSDB.TAG.MOLID, MSDB.TAG.MZ, MSDB.TAG.RT, MSDB.TAG.COL, MSDB.TAG.COLRT, MSDB.TAG.ATTR, MSDB.TAG.COMP) %in% colnames(z)))
}

test.peak.output.file <- function() {
	call.search.mz(c('-m pos', '-c uplc-c8,uplc-c18', '--rttolx 5', '--rttoly 0.8', '-i', 'mzrt-input-small.tsv', '-o', 'mzrt-output.tsv', '--same-rows', '--peak-output-file', 'mzrt-output-peaks.tsv'))
	x <- read.table(get.res.path('mzrt-input-small.tsv'), header = TRUE)
	y <- read.table(get.res.path('mzrt-output.tsv'), header = TRUE)
	z <- read.table(get.res.path('mzrt-output-peaks.tsv'), header = TRUE)
	checkTrue(nrow(x) == nrow(y))
	checkTrue(all(c('mz', 'rt') %in% colnames(z)))
}

test.html.output.file <- function() {
	call.search.mz(c('-m pos', '-i', 'mzrt-input-small.tsv', '-o', 'mzrt-output.tsv', '--peak-output-file', 'mzrt-output-peaks.tsv', '--html-output-file', 'mzrt-output.html'))
	checkTrue(file.exists(get.res.path('mzrt-output.html')))
}
