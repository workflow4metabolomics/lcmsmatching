if ( ! exists('remove.na.rows')) { # Do not load again if already loaded

	source('strhlp.R')

	#################
	# RENAME COLUMN #
	#################

	rename.col <- function(df, cur, new) {

		for (k in seq(cur)) {
			i <- which(cur[k] == colnames(df)) 
			if (length(i) == 1)
				colnames(df)[i] <- new[k]
		}

		return(df)
	}

	##################
	# REMOVE NA ROWS #	
	##################

	remove.na.rows <- function(df) {
		na.rows <- apply(is.na(df), MARGIN = 1, all)
		return(df[ ! na.rows, , drop = FALSE])
	}

	######################
	# MOVE COLUMNS FIRST #
	######################

	df.move.col.first <- function(df, cols) {
		not.cols <- setdiff(names(df), cols)
		df[c(cols, not.cols)]
	}

	#####################
	# MOVE COLUMNS LAST #
	#####################

	df.move.col.last <- function(df, cols) {
		not.cols <- setdiff(names(df), cols)
		df[c(not.cols, cols)]
	}

	##############
	# READ TABLE #
	##############

	df.read.table <- function(file, sep = "", header = TRUE, remove.na.rows = TRUE, check.names = TRUE, stringsAsFactors = TRUE, trim.header = FALSE, trim.values = FALSE, fileEncoding = "") {

		# Call built-in read.table()
		df <- read.table(file, sep = sep, header = header, check.names = check.names, stringsAsFactors = stringsAsFactors, fileEncoding = fileEncoding)

		# Clean data frame
		df <- df.clean(df, trim.colnames = trim.header, trim.values = trim.values, remove.na.rows = remove.na.rows)

		return(df)
	}

	#################
	# READ CSV FILE #
	#################

	# Read CSV file and return a data.frame.
	# file      The path to the CSV file.
	# header                If TRUE, use first line as header line.
	# check.names           If TRUE, correct header (column) names in the data frame, by replacing non-ASCII characters by dot.
	# stringsAsFactors      If TRUE, replace string values by factors.
	# trim.header           If TRUE, remove whitespaces at beginning and of header titles.
	# trim.values           If TRUE, remove whitespaces at beginning and of string values.
	# remove.na.rows        If TRUE, remove all lines that contain only NA values.
	df.read.csv <- function(file, header = TRUE, remove.na.rows = TRUE, check.names = TRUE, stringsAsFactors = TRUE, trim.header = FALSE, trim.values = FALSE) {
	
		# Call built-in read.csv()
		df <- read.csv(file, header = header, check.names = check.names, stringsAsFactors = stringsAsFactors)

		# Clean data frame
		df <- df.clean(df, trim.colnames = trim.header, trim.values = trim.values, remove.na.rows = remove.na.rows)

		return(df)
	}

	##################
	# WRITE TSV FILE #
	##################

	df.write.tsv <- function(df, file, row.names = FALSE, col.names = TRUE) {
		write.table(df, file = file, row.names = row.names, col.names = col.names, sep = "\t")
	}

	####################
	# CLEAN DATA FRAME #
	####################

	df.clean <- function(df, trim.colnames = FALSE, trim.values = FALSE, remove.na.rows = FALSE) {

		# Remove NA lines
		if (remove.na.rows)
			df <- remove.na.rows(df)

		# Trim header
		if (trim.colnames)
			colnames(df) <- trim(colnames(df))

		# Trim values
		if (trim.values)
			for (c in 1:ncol(df))
				if (typeof(df[[c]]) == 'character')
					df[[c]] <- trim(df[[c]])
		
		return(df)
	}

} # end of load safe guard
