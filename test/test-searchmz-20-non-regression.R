test.searchmz.ticket.2016031010000034 <- function() {
	call.search.mz(c('-d', 'file', '--url', 'res/ticket-2016031010000034-database_CEA_test_2_utf8.tsv', '-m', 'pos', '-i', 'res/ticket-2016031010000034-input_file_for_db_test_2.tsv', '--input-col-names', 'mz=mzmed,rt=rtmed', '-o', 'ticket-2016031010000034-output.tsv', '--peak-output-file', 'ticket-2016031010000034-output-peaks.tsv', '--html-output-file', 'ticket-2016031010000034-output.html'), use.global.conn.flags = FALSE)
}

test.searchmz.peakforest.estelle.20170314 <- function() {
	call.search.mz(c('-d', 'peakforest', '--url', 'https://peakforest-alpha.inra.fr/rest', '--db-token', ENV[['RMSBD_PEAKFOREST_TOKEN']], '-m', 'pos', '-p', '5', '-s', '0', '-i', 'res/peakforest.estelle.20170314-input.tsv', '--input-col-names', 'mz=mzmed,rt=rtmed', '-o', 'peakforest.estelle.20170314-output.tsv', '--peak-output-file', 'peakforest.estelle.20170314-output-peaks.tsv', '--html-output-file', 'peakforest.estelle.20170314-output.html'), use.global.conn.flags = FALSE)
}

test.searchmz.peakforest.estelle.20170316.rtunit <- function() {

	res.name <- '2017-03-16-estelle-rtunit'
	res.dir <- file.path(dirname(script.path), 'res', res.name)
	min.input <- file.path(res.dir, 'min-input.tsv')
	min.main.output <- file.path(dirname(script.path), paste(res.name, 'min', 'main.tsv', sep = '-'))
	min.peak.output <- file.path(dirname(script.path), paste(res.name, 'min', 'peak.tsv', sep = '-'))
	min.html.output <- file.path(dirname(script.path), paste(res.name, 'min.html', sep = '-'))
	sec.input <- file.path(res.dir, 'sec-input.tsv')
	sec.main.output <- file.path(dirname(script.path), paste(res.name, 'sec', 'main.tsv', sep = '-'))
	sec.peak.output <- file.path(dirname(script.path), paste(res.name, 'sec', 'peak.tsv', sep = '-'))
	sec.html.output <- file.path(dirname(script.path), paste(res.name, 'sec.html', sep = '-'))

	call.search.mz(c('-d', 'peakforest', '--url' ,'https://peakforest-alpha.inra.fr/rest', '--db-token', ENV[['RMSBD_PEAKFOREST_TOKEN']], '-m', 'pos', '-p', '5', '-s', '0', '-i', min.input, '--input-col-names', 'mz=mzmed,rt=rtmed', '-o', min.main.output, '--peak-output-file', min.peak.output, '--html-output-file', min.html.output, '--rtunit', 'min', '--all-cols', '--rttolx', '5', '--rttoly', '0.8', '--check-cols', '--same-rows', '--same-cols', '--no-main-table-in-html-output'), use.global.conn.flags = FALSE)

	call.search.mz(c('-d', 'peakforest', '--url' ,'https://peakforest-alpha.inra.fr/rest', '--db-token', ENV[['RMSBD_PEAKFOREST_TOKEN']], '-m', 'pos', '-p', '5', '-s', '0', '-i', sec.input, '--input-col-names', 'mz=mzmed,rt=rtmed', '-o', sec.main.output, '--peak-output-file', sec.peak.output, '--html-output-file', sec.html.output, '--rtunit', 'sec', '--all-cols', '--rttolx', '5', '--rttoly', '0.8', '--check-cols', '--same-rows', '--same-cols', '--no-main-table-in-html-output'), use.global.conn.flags = FALSE)

	# Check that output file contains at least one match
	sec.peaks <- read.table(sec.peak.output, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
	min.peaks <- read.table(min.peak.output, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
	checkTrue(MSDB.TAG.MOLID %in% colnames(sec.peaks))
	checkTrue(any( ! is.na(sec.peaks[[MSDB.TAG.MOLID]])))
	checkTrue(MSDB.TAG.MOLID %in% colnames(min.peaks))
	checkTrue(any( ! is.na(min.peaks[[MSDB.TAG.MOLID]])))
	checkTrue(nrow(sec.peaks) == nrow(min.peaks))
	checkTrue(ncol(sec.peaks) == ncol(min.peaks))
	checkIdentical(sec.peaks[[MSDB.TAG.MOLID]], min.peaks[[MSDB.TAG.MOLID]])

	# Checkout RT output values
	min.in <- read.table(min.input, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
	sec.in <- read.table(sec.input, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
	checkTrue(nrow(min.in) == 1)
	checkTrue(nrow(sec.in) == 1)
	checkTrue('rtmed' %in% colnames(min.peaks))
	checkTrue('rtmed' %in% colnames(sec.peaks))
	checkTrue(all(abs(min.peaks[['rtmed']] - min.in[['rtmed']]) < 1e-10))
	checkTrue(all(abs(sec.peaks[['rtmed']] - sec.in[['rtmed']]) < 1e-10))
	sec.main <- read.table(sec.main.output, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
	min.main <- read.table(min.main.output, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
	checkTrue('rtmed' %in% colnames(min.main))
	checkTrue('rtmed' %in% colnames(sec.main))
	checkTrue(all(abs(min.main[['rtmed']] - min.in[['rtmed']]) < 1e-10))
	checkTrue(all(abs(sec.main[['rtmed']] - sec.in[['rtmed']]) < 1e-10))
}

test.searchmz.filedb.rtunit <- function() {

	res.name <- 'filedb'
	res.dir <- file.path(dirname(script.path), 'res', res.name)
	nb.match <- NA_integer_
	for (db.rtunit in MSDB.RTUNIT.VALS) {

		file.db <- file.path(res.dir, paste('filedb-rt', db.rtunit, '.tsv', sep = ''))

		for (input.rtunit in MSDB.RTUNIT.VALS) {

			input.file <- file.path(res.dir, paste('mzrt-input-', input.rtunit, '.tsv', sep = ''))

			main.output <- file.path(dirname(script.path), paste(res.name, db.rtunit, 'input', input.rtunit, 'main.tsv', sep = '-'))
			peak.output <- file.path(dirname(script.path), paste(res.name, db.rtunit, 'input', input.rtunit, 'peak.tsv', sep = '-'))
			html.output <- file.path(dirname(script.path), paste(res.name, db.rtunit, 'input', input.rtunit, 'peak.html', sep = '-'))

			call.search.mz(c('-d', 'file', '--url', file.db, '--db-rt-unit', db.rtunit, '-m', 'pos', '-p', '5', '-s', '0', '-i', input.file, '--input-col-names', 'mz=mz,rt=rt', '-o', main.output, '--peak-output-file', peak.output, '--html-output-file', html.output, '--rtunit', input.rtunit, '--all-cols', '--rttolx', '5', '--rttoly', '0.8', '--check-cols', '--same-rows', '--same-cols', '--no-main-table-in-html-output'), use.global.conn.flags = FALSE)

			# Check outputs
			main <- read.table(main.output, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
			peak <- read.table(peak.output, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
			checkTrue(any( ! is.na(main[[MSDB.TAG.MOLID]])))
			checkTrue(any( ! is.na(peak[[MSDB.TAG.MOLID]])))
			if (is.na(nb.match))
				nb.match <- sum(! is.na(peak[[MSDB.TAG.MOLID]]))
			else
				checkEquals(sum(! is.na(peak[[MSDB.TAG.MOLID]])), nb.match)
		}
	}
}
