if ( ! exists('read.excel')) { # Do not load again if already loaded
	
	source('strhlp.R')
	source('dfhlp.R')
	
	###############
	# GET NB ROWS #
	###############
	
	get.nbrows <- function(file, tab) {
	
		library(rJava)
		library(xlsxjars)
		library(xlsx, quietly = TRUE)
	
		df <- read.xlsx(file, tab)
		na_rows <- apply(is.na(df), MARGIN = 1, FUN = all) # look for rows that contain only NA values.
		last_row <- tail(which(! na_rows), n = 1)
		return(last_row)
	}

	##############
	# READ EXCEL #
	##############
	
	# Read Excel xlsx file
	# file                  The path to the Excel file.
	# sheet
	# start.row
	# end.row
	# header                If TRUE, use first line as header line.
	# check.names           If TRUE, correct header (column) names in the data frame, by replacing non-ASCII characters by dot.
	# stringsAsFactors      If TRUE, replace string values by factors.
	# trim.header           If TRUE, remove whitespaces at beginning and of header titles.
	# trim.values           If TRUE, remove whitespaces at beginning and of string values.
	# remove.na.rows        If TRUE, remove all lines that contain only NA values.
	read.excel <- function(file, sheet, start.row = NULL, end.row = NULL, header = TRUE, remove.na.rows = TRUE, check.names = TRUE, stringsAsFactors = TRUE, trim.header = FALSE, trim.values = FALSE, col.index = NULL) {
	
		library(rJava)
		library(xlsxjars)
		library(xlsx, quietly = TRUE)
	
		# Check that start row and end row exist
		if ( ! is.null(start.row) || ! is.null(end.row)) {
			nb_rows <- get.nbrows(file, sheet)
			if ( ! is.null(start.row) && start.row > nb_rows)
				return(NULL)
			if ( ! is.null(end.row) && end.row > nb_rows)
				return(NULL)
		}
	
		# Call xlsx package
		df <- read.xlsx(file, sheet, startRow = start.row, endRow = end.row, header = header, check.names = check.names, stringsAsFactors = stringsAsFactors, colIndex = col.index)

		# Remove column default names if header was set to false
		if ( ! header)
			colnames(df) <- NULL

		# Clean data frame
		df <- df.clean(df, trim.colnames = trim.header, trim.values = trim.values, remove.na.rows = remove.na.rows)
	
		return(df)
	}
	
	#######################
	# CHECK IF TAB EXISTS #
	#######################
	
	tab.exists <- function(file, tab) {
	
		if (is.null(file) || is.na(file) || is.null(tab) || is.na(tab))
			return(FALSE)
	
		library(rJava)
		library(xlsxjars)
		library(xlsx, quietly = TRUE)

		wb <- loadWorkbook(file)
		sheets <- getSheets(wb)
		return(tab %in% names(sheets))
	}

} # end of load safe guard
