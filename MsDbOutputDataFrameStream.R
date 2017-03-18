if ( ! exists('MsDbOutputDataFrameStream')) { # Do not load again if already loaded

	library(methods)
	source('MsDbOutputStream.R')
	source('dfhlp.R', chdir = TRUE)

	#####################
	# CLASS DECLARATION #
	#####################
	
	MsDbOutputDataFrameStream <- setRefClass("MsDbOutputDataFrameStream", contains = 'MsDbOutputStream', fields = list( .df = "ANY"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	MsDbOutputDataFrameStream$methods( initialize = function(keep.unused = FALSE, one.line = FALSE, match.sep = MSDB.DFT.MATCH.SEP, output.fields = msdb.get.dft.output.fields(), multval.field.sep = MSDB.DFT.OUTPUT.MULTIVAL.FIELD.SEP, first.val = FALSE, ascii = FALSE, noapostrophe = FALSE, noplusminus = FALSE, nogreek = FALSE, ...) {
		
		callSuper(keep.unused = keep.unused, one.line = one.line, match.sep = match.sep, output.fields = output.fields, multval.field.sep = multval.field.sep, first.val = first.val, ascii = ascii, noapostrophe = noapostrophe, noplusminus = noplusminus, nogreek = nogreek, ...)

		.df <<- data.frame()
	})

	##################
	# GET DATA FRAME #
	##################
	
	MsDbOutputDataFrameStream$methods( getDataFrame = function() {

		# Put at least a column name if empty
		if (nrow(.self$.df) == 0)
			.self$.df[[.self$.output.fields[[MSDB.TAG.MZ]]]] <- numeric()

		return(.self$.df)
	})
	
	# Move columns to beginning {{{1
	
	MsDbOutputDataFrameStream$methods( moveColumnsToBeginning = function(cols) {
		all.cols <- colnames(.self$.df)
		other.cols <- all.cols[ ! all.cols %in% cols]
		cols <- cols[cols %in% all.cols]
		.df <<- .self$.df[c(cols, other.cols)]
	})

	#################
	# MATCHED PEAKS #
	#################
	
	MsDbOutputDataFrameStream$methods( matchedPeaks = function(mz, rt = NULL, unused = NULL, peaks = NULL) {

		library(plyr)

		# Set input values
		x <- data.frame(mz = mz)
		colnames(x) <- MSDB.TAG.MZ
		if ( ! is.null(rt)) {
			x.rt <- data.frame(rt = rt)
			colnames(x.rt) <- MSDB.TAG.RT
			if (.self$.rtunit == MSDB.RTUNIT.MIN)
				x.rt[[MSDB.TAG.RT]] <- x.rt[[MSDB.TAG.RT]] / 60
			x <- cbind(x, x.rt)
		}


		# Merge input values with matched peaks
		if ( ! is.null(peaks)) {

			# No rows
			if (nrow(peaks) == 0) {
				# Add NA values
				peaks[1, ] <- NA

			# Process existing rows
			} else {

				# Convert RT
				if (.self$.rtunit == MSDB.RTUNIT.MIN)
					if (MSDB.TAG.COLRT %in% colnames(peaks))
						peaks[[MSDB.TAG.COLRT]] <- peaks[[MSDB.TAG.COLRT]] / 60

				# Process multi-value fields
				for (c in colnames(peaks))
					if (c %in% MSDB.MULTIVAL.FIELDS) {

						# Keep only first value in multi-value fields
						if (.self$.first.val)
							peaks[[c]] <- vapply(peaks[[c]], function(s) split.str(s, sep = MSDB.MULTIVAL.FIELD.SEP, unlist = TRUE)[[1]], FUN.VALUE = '')

						# Change separator
						else
							peaks[[c]] <- vapply(peaks[[c]], function(s) paste0(split.str(s, sep = MSDB.MULTIVAL.FIELD.SEP, unlist = TRUE), collapse = .self$.multval.field.sep), FUN.VALUE = '')

					}

				# Concatenate results in one line
				if (.self$.one.line) {
 					# For each column, concatenate all values in one string.
					for (c in seq(peaks)) {
						v <- peaks[[c]]
						v <- v[ ! is.na(v)] # remove NA values
						v <- v[ ! duplicated(v)] # remove duplicates
						peaks[1, c] <- paste0(v, collapse = .self$.match.sep, FUN.VALUE = '')
					}
					peaks <- peaks[1, ] # Keep only first line
				}
			}

			# Merge
			x <- cbind(x, peaks, row.names = NULL)
		}

		# Rename columns for output
		x <- rename.col(x, names(.self$.output.fields), .self$.output.fields)

		# Add unused columns
		if ( .self$.keep.unused && ! is.null(unused)) {
			x <- cbind(x, unused, row.names = NULL)
		}

		# Convert strings to ASCII
		if (.self$.ascii || .self$.noapostrophe || .self$.noplusminus || .self$.nogreek)
			for (c in seq(x))
				if (class(x[[c]]) == 'character') {
					if (.self$.noapostrophe)
						x[[c]] <- gsub("'", 'prime', x[[c]], perl = TRUE)
					if (.self$.noplusminus)
						x[[c]] <- gsub('±', '+-', x[[c]], perl = TRUE)
					if (.self$.nogreek) {
						x[[c]] <- gsub('α', 'alpha', x[[c]], perl = TRUE)
						x[[c]] <- gsub('β', 'beta', x[[c]], perl = TRUE)
						x[[c]] <- gsub('γ', 'gamma', x[[c]], perl = TRUE)
						x[[c]] <- gsub('δ', 'delta', x[[c]], perl = TRUE)
					}
					if (.self$.ascii) {
						x[[c]] <- gsub('[^\u0001-\u007F]', '_', x[[c]], perl = TRUE)
					}
				}

		# Add new rows to data frame
		.df <<- rbind.fill(.self$.df, x)
	})

} # end of load safe guard
