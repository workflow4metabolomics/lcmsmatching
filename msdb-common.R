if ( ! exists('msdb.get.dft.input.fields')) { # Do not load again if already loaded

#	library('stringr')
	source('biodb-common.R', chdir = TRUE)

	#############
	# CONSTANTS #
	#############

	# Field tags
	MSDB.TAG.MZ <- 'mz'
	MSDB.TAG.MZEXP <- 'mzexp'
	MSDB.TAG.MZTHEO <- 'mztheo'
	MSDB.TAG.RT <- 'rt'
	MSDB.TAG.MODE <- 'msmode'
	MSDB.TAG.MOLID <- 'compoundid'
	MSDB.TAG.COL <- 'chromcol'
	MSDB.TAG.COLRT <- 'chromcolrt'
	MSDB.TAG.ATTR <- 'peakattr'
	MSDB.TAG.INT <- 'intensity'
	MSDB.TAG.REL <- 'relative.intensity'
	MSDB.TAG.COMP <- 'peakcomp'
	MSDB.TAG.MOLNAMES <- 'fullnames'
	MSDB.TAG.MOLCOMP <- 'compoundcomp'
	MSDB.TAG.MOLMASS <- 'compoundmass'
	MSDB.TAG.INCHI <- 'inchi'
	MSDB.TAG.INCHIKEY <- 'inchikey'
	MSDB.TAG.PUBCHEM <- 'pubchemcompid'
	MSDB.TAG.CHEBI <- 'chebiid'
	MSDB.TAG.HMDB <- 'hmdbid'
	MSDB.TAG.KEGG <- 'keggid'

	# Mode tags
	MSDB.TAG.POS           <- 'pos'
	MSDB.TAG.NEG           <- 'neg'

#	# Fields containing multiple values
#	MSDB.MULTIVAL.FIELDS <- c(MSDB.TAG.MOLNAMES)
#	MSDB.MULTIVAL.FIELD.SEP <- ';'

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
#	MSDB.DFT.OUTPUT.MULTIVAL.FIELD.SEP <- MSDB.MULTIVAL.FIELD.SEP
	MSDB.DFT.MATCH.FIELDS <- list( molids = 'molid', molnames = 'molnames')
	MSDB.DFT.MATCH.SEP <- '|'
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

} # end of load safe guard
