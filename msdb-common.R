if ( ! exists('.parse_chrom_col_desc')) { # Do not load again if already loaded

	library('stringr')
	source('strhlp.R', chdir = TRUE)
	source('biodb-common.R', chdir = TRUE)

	#############
	# CONSTANTS #
	#############

	# Field tags
	MSDB.TAG.MZ <- BIODB.PEAK.MZ
	MSDB.TAG.MZEXP <- BIODB.PEAK.MZEXP
	MSDB.TAG.MZTHEO <- BIODB.PEAK.MZTHEO
	MSDB.TAG.RT <- BIODB.PEAK.RT
	MSDB.TAG.MODE <- BIODB.MSMODE
	MSDB.TAG.MOLID <- BIODB.COMPOUND.ID
	MSDB.TAG.COL <- BIODB.CHROM.COL
	MSDB.TAG.COLRT <- BIODB.CHROM.COL.RT
	MSDB.TAG.ATTR <- BIODB.PEAK.ATTR
	MSDB.TAG.INT <- BIODB.PEAK.INTENSITY
	MSDB.TAG.REL <- BIODB.PEAK.RELATIVE.INTENSITY
	MSDB.TAG.COMP <- BIODB.PEAK.COMP
	MSDB.TAG.MOLNAMES <- BIODB.FULLNAMES
	MSDB.TAG.MOLCOMP <- BIODB.COMPOUND.MASS
#	MSDB.TAG.MOLATTR <- 'molattr'
	MSDB.TAG.MOLMASS <- BIODB.COMPOUND.COMP
	MSDB.TAG.INCHI <- BIODB.INCHI
	MSDB.TAG.INCHIKEY <- BIODB.INCHIKEY
	# TODO Use BIODB tags.
	MSDB.TAG.PUBCHEM <- BIODB.PUBCHEMCOMP.ID
	MSDB.TAG.CHEBI <- BIODB.CHEBI.ID
	MSDB.TAG.HMDB <- BIODB.HMDB.ID
	MSDB.TAG.KEGG <- BIODB.KEGG.ID

	# Mode tags
	MSDB.TAG.POS           <- BIODB.MSMODE.NEG
	MSDB.TAG.NEG           <- BIODB.MSMODE.POS

	# Fields containing multiple values
	MSDB.MULTIVAL.FIELDS <- c(MSDB.TAG.MOLNAMES)
	MSDB.MULTIVAL.FIELD.SEP <- ';'

	# Authorized mz tolerance unit values
	MSDB.MZTOLUNIT.PPM <- 'ppm'
	MSDB.MZTOLUNIT.PLAIN <- 'plain' # same as mz: mass-to-charge ratio
	MSDB.MZTOLUNIT.VALS <- c(MSDB.MZTOLUNIT.PPM, MSDB.MZTOLUNIT.PLAIN)

	# Authorized rt units
	MSDB.RTUNIT.SEC <- 'sec'
	MSDB.RTUNIT.MIN <- 'min'
	MSDB.RTUNIT.VALS <- c(MSDB.RTUNIT.SEC ,MSDB.RTUNIT.MIN)

	# Default values
	MSDB.DFT.PREC <- list()
	MSDB.DFT.PREC[[MSDB.TAG.POS]] <- c("[(M+H)]+", "[M+H]+", "[(M+Na)]+", "[M+Na]+", "[(M+K)]+", "[M+K]+")
	MSDB.DFT.PREC[[MSDB.TAG.NEG]] <- c("[(M-H)]-", "[M-H]-", "[(M+Cl)]-", "[M+Cl]-")
#	MSDB.DFT.OUTPUT.FIELDS <- list( mz = 'mz', rt = 'rt', col = 'col', colrt = 'colrt', molid = 'id', attr = 'attribution', comp = 'composition', int = 'intensity', rel = 'relative', mzexp = 'mzexp', mztheo = 'mztheo', msmatching = 'msmatching', molnames = 'molnames', molcomp = 'molcomp', molmass = 'molmass', inchi = 'inchi', inchikey = 'inchikey', pubchem = 'pubchem', chebi = 'chebi', hmdb = 'hmdb', kegg = 'kegg')
	MSDB.DFT.OUTPUT.MULTIVAL.FIELD.SEP <- MSDB.MULTIVAL.FIELD.SEP
	MSDB.DFT.MATCH.FIELDS <- list( molids = 'molid', molnames = 'molnames')
	MSDB.DFT.MATCH.SEP <- ','
	MSDB.DFT.MODES <- list( pos = 'POS', neg = 'NEG')
	MSDB.DFT.MZTOLUNIT <- MSDB.MZTOLUNIT.PPM

	############################
	# GET DEFAULT INPUT FIELDS #
	############################

	msdb.get.dft.input.fields <- function () {

		dft.fields <- list()

		for(f in c(MSDB.TAG.MZ, MSDB.TAG.RT))
			dft.fields[[f]] <- f

		return(dft.fields)
	}

	#############################
	# GET DEFAULT OUTPUT FIELDS #
	#############################

	msdb.get.dft.output.fields <- function () {

		dft.fields <- list()

		for(f in c(MSDB.TAG.MZ, MSDB.TAG.RT, MSDB.TAG.COL, MSDB.TAG.COLRT, MSDB.TAG.MOLID, MSDB.TAG.ATTR, MSDB.TAG.COMP, MSDB.TAG.INT, MSDB.TAG.REL, MSDB.TAG.MZEXP, MSDB.TAG.MZTHEO, MSDB.TAG.MOLNAMES, MSDB.TAG.MOLCOMP, MSDB.TAG.MOLMASS, MSDB.TAG.INCHI, MSDB.TAG.INCHIKEY, MSDB.TAG.PUBCHEM, MSDB.TAG.CHEBI, MSDB.TAG.HMDB, MSDB.TAG.KEGG))
			dft.fields[[f]] <- f

		return(dft.fields)
	}

	#########################
	# GET DEFAULT DB FIELDS #
	#########################

	msdb.get.dft.db.fields <- function () {

		dft.fields <- list()

		for (f in c(MSDB.TAG.MZTHEO, MSDB.TAG.COLRT, MSDB.TAG.MOLID, MSDB.TAG.COL, MSDB.TAG.MODE, MSDB.TAG.ATTR, MSDB.TAG.COMP, MSDB.TAG.MOLNAMES, MSDB.TAG.MOLCOMP, MSDB.TAG.MOLMASS, MSDB.TAG.INCHI, MSDB.TAG.INCHIKEY, MSDB.TAG.PUBCHEM, MSDB.TAG.CHEBI, MSDB.TAG.HMDB, MSDB.TAG.KEGG))
			dft.fields[[f]] <- f

		return(dft.fields)
	}

	##################
	# MAKE DB FIELDS #
	##################

	msdb.make.db.fields <- function(fields) {

		# Merge with default fields
		dft.fields <- msdb.get.dft.db.fields()
		absent <- ! names(dft.fields) %in% names(fields)
		if (length(absent) > 0)
			fields <- c(fields, dft.fields[absent])

		return(fields)
	}

	#########################
	# MAKE INPUT DATA FRAME #
	#########################

	msdb.make.input.df <- function(mz, rt = NULL, rtunit = MSDB.RTUNIT.SEC) {

		field <- msdb.get.dft.input.fields()

		x <- data.frame()

		# Set mz
		if (length(mz) > 1)
			x[seq(mz), field[[MSDB.TAG.MZ]]] <- mz
		else if (length(mz) == 1)
			x[1, field[[MSDB.TAG.MZ]]] <- mz
		else
			x[, field[[MSDB.TAG.MZ]]] <- numeric()

		# Set rt
		if ( ! is.null(rt)) {
			if (rtunit == MSDB.RTUNIT.MIN)
				rtunit <- rtunit * 60
			if (length(rt) > 1)
				x[seq(rt), field[[MSDB.TAG.RT]]] <- rt
			else if (length(rt) == 1)
				x[1, field[[MSDB.TAG.RT]]] <- rt
			else
				x[, field[[MSDB.TAG.RT]]] <- numeric()
		}

		return(x)
	}

	###############################
	# GET EMPTY RESULT DATA FRAME #
	###############################

	.get.empty.result.df <- function(rt = FALSE) {

		df <- data.frame(stringsAsFactors = FALSE)
		df[MSDB.TAG.MOLID] <- character()
 		df[MSDB.TAG.MOLNAMES] <- character()
 		df[MSDB.TAG.MZ] <- numeric()
 		df[MSDB.TAG.MZTHEO] <- numeric()
 		df[MSDB.TAG.ATTR] <- character()
 		df[MSDB.TAG.COMP] <- character()
		if (rt) {
 			df[MSDB.TAG.RT] <- numeric()
 			df[MSDB.TAG.COL] <- character()
 			df[MSDB.TAG.COLRT] <- numeric()
		}

		return(df)
	}

	############################
	# PARSE COLUMN DESCRIPTION #
	############################
	
	.parse_chrom_col_desc <- function(desc) {
	
		# Clean string
		s <- desc
		s <- gsub('\\.+', ' ', s, perl = TRUE) # Replace '.' characters by spaces
		s <- gsub('[*-]', ' ', s, perl = TRUE) # Replace dashes and asterisks by spaces
		s <- gsub('[)(]', '', s, perl = TRUE) # Remove paranthesis
		s <- trim(s)
		s <- tolower(s) # put in lowercase
		
		# Match      2                         3 4                   5 6         7 8                                           9 10        1112         13
		pattern <- "^(uplc|hsf5|hplc|zicphilic)( (c8|c18|150 5 2 1))?( (\\d+)mn)?( (orbitrap|exactive|qtof|shimadzu exactive))?( (\\d+)mn)?( (bis|ter))?( 1)?$"
		g <- str_match(s, pattern)
		if (is.na(g[1, 1]))
			stop(paste0("Impossible to parse column description \"", desc, "\"."))

		type <- g[1, 2]
		stationary_phase <- if ( ! is.na(g[1, 4]) && nchar(g[1, 4]) > 0) g[1, 4] else NA_character_
		msdevice <- if ( ! is.na(g[1, 8]) && nchar(g[1, 8]) > 0) g[1, 8] else NA_character_
		time <- if ( ! is.na(g[1,6]) && nchar(g[1, 6]) > 0) as.integer(g[1, 6]) else ( if ( ! is.na(g[1, 10]) && nchar(g[1, 10]) > 0) as.integer(g[1, 10]) else NA_integer_ )
		
		# Correct values
		if ( ! is.na(stationary_phase) && stationary_phase == '150 5 2 1') stationary_phase <- '150*5*2.1'
		if ( ! is.na(msdevice)) msdevice <- gsub(' ', '', msdevice) # remove spaces

		return(list( type = type, stationary_phase = stationary_phase, time = time, msdevice = msdevice))

	}
	
	#########################
	# NORMALIZE COLUMN NAME #
	#########################

	.normalize_column_name <- function(desc) {
	
		lst <- .parse_chrom_col_desc(desc)
	
		v <- c(lst$type)
		if ( ! is.na(lst$stationary_phase))
			v <- c(v, lst$stationary_phase)
		if ( ! is.na(lst$time))
			v <- c(v, paste0(lst$time, "min"))
		if ( ! is.na(lst$msdevice))
			v <- c(v, lst$msdevice)
	
		return(paste(v, collapse = '-'))
	}

} # end of load safe guard
