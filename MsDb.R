if ( ! exists('MsDb')) { # Do not load again if already loaded
	
	library('methods')
	source('msdb-common.R')
	source('MsDbObserver.R')
	source('MsDbOutputStream.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	MsDb <- setRefClass("MsDb", fields = list(.observers = "ANY", .prec = "list", .output.streams = "ANY", .input.stream = "ANY", .mz.tol.unit = "character", .rt.unit = "character"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	MsDb$methods( initialize = function(...) {
		
		callSuper(...)

		.observers <<- NULL
		.output.streams <<- NULL
		.input.stream <<- NULL
		.prec <<- MSDB.DFT.PREC
		.mz.tol.unit <<- MSDB.DFT.MZTOLUNIT
		.rt.unit <<- MSDB.RTUNIT.SEC
	})

	####################
	# SET INPUT STREAM #
	####################
	
	MsDb$methods( setInputStream = function(stream) {
	
		# Check types of input stream
		if ( ! inherits(stream, "MsDbInputStream") && ! is.null(stream))
			stop("Input stream must inherit from MsDbInputStream class.")
	
		# Save current stream
		cur.stream <- .self$.input.stream

		# Set stream
		.input.stream <<- stream

		return(cur.stream)
	})

	######################
	# ADD OUTPUT STREAMS #
	######################
	
	MsDb$methods( addOutputStreams = function(stream) {
	
		# Check types of output streams
		if ( ( ! is.list(stream) && ! inherits(stream, "MsDbOutputStream")) || (is.list(stream) && any( ! vapply(stream, function(s) inherits(s, "MsDbOutputStream"), FUN.VALUE = TRUE))))
			stop("Output streams must inherit from MsDbOutputStream class.")
	
		# Add streams to current list
		.output.streams <<- if (is.null(.self$.output.streams)) c(stream) else c(.self$.output.streams, stream)
	})

	#########################
	# REMOVE OUTPUT STREAMS #
	#########################
	
	MsDb$methods( removeOutputStreams = function(stream) {
	
		# Check types of output streams
		if ( ( ! is.list(stream) && ! inherits(stream, "MsDbOutputStream")) || (is.list(stream) && any( ! vapply(stream, function(s) inherits(s, "MsDbOutputStream"), FUN.VALUE = TRUE))))

		# Remove streams from current list
		.output.streams <<- .self$.output.streams[ ! stream %in% .self$.output.streams]
	})

	########################
	# RESET OUTPUT STREAMS #
	########################
	
	MsDb$methods( resetOutputStreams = function(stream) {
		.output.streams <<- NULL
	})

	#################
	# ADD OBSERVERS #
	#################
	
	MsDb$methods( addObservers = function(obs) {
	
		# Check types of observers
		if ( ( ! is.list(obs) && ! inherits(obs, "MsDbObserver")) || (is.list(obs) && any( ! vapply(obs, function(o) inherits(o, "MsDbObserver"), FUN.VALUE = TRUE))))
			stop("Observers must inherit from MsDbObserver class.")
	
		# Add observers to current list
		.observers <<- if (is.null(.self$.observers)) c(obs) else c(.self$.observers, obs)
	})
	
	##################
	# SET PRECURSORS #
	##################
	
	MsDb$methods( setPrecursors = function(prec) {
		.prec <<- prec
	})
	
	#################
	# SET DB FIELDS #
	#################
	
	MsDb$methods( areDbFieldsSettable = function() {
		return(FALSE)
	})
	
	MsDb$methods( setDbFields = function(fields) {
		stop("Method setDbFields() not implemented in concrete class.")
	})
	
	################
	# SET MS MODES #
	################
	
	MsDb$methods( areDbMsModesSettable = function() {
		return(FALSE)
	})
	
	MsDb$methods( setDbMsModes = function(modes) {
		stop("Method setDbMsModes() not implemented in concrete class.")
	})
	
	###################
	# SET MZ TOL UNIT #
	###################

	MsDb$methods( setMzTolUnit = function(mztolunit) {

		if ( ! mztolunit %in% MSDB.MZTOLUNIT.VALS)
			stop(paste0("M/Z tolerance unit must be one of: ", paste(MSDB.MZTOLUNIT.VALS, collapse = ', '), "."))

		.mz.tol.unit <<- mztolunit
	})

	###############
	# SET RT UNIT #
	###############

	MsDb$methods( setRtUnit = function(unit) {

		if ( ! unit %in% MSDB.RTUNIT.VALS)
			stop(paste0("RT unit must be one of: ", paste(MSDB.RTUNIT.VALS, collapse = ', '), "."))

		.rt.unit <<- unit
	})

	###############
	# GET RT UNIT #
	###############

	MsDb$methods( getRtUnit = function(unit) {
		return(.self$.rt.unit)
	})

	####################
	# HANDLE COMPOUNDS #
	####################
	
	# Returns TRUE if this database handles compounds directly (by IDs)
	MsDb$methods( handleCompounds = function() {
		return(TRUE)
	})

	####################
	# GET MOLECULE IDS #
	####################
	
	# Returns an integer vector of all molecule IDs stored inside the database.
	MsDb$methods( getMoleculeIds = function(max.results = NA_integer_) {
		stop("Method getMoleculeIds() not implemented in concrete class.")
	})

	####################
	# GET NB MOLECULES #
	####################
	
	# Returns the number of molecules in the database.
	MsDb$methods( getNbMolecules = function() {
		stop("Method getNbMolecules() not implemented in concrete class.")
	})
	
	#################
	# GET MZ VALUES #
	#################
	
	# Returns a numeric vector of all masses stored inside the database.
	MsDb$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {
		stop("Method getMzValues() not implemented in concrete class.")
	})
	
	#####################
	# GET MOLECULE NAME #
	#####################
	
	# Get molecule names
	# molid     An integer vector of molecule IDs.
	# Returns a character vector containing the names of the molecule IDs, in the same order as the input vector.
	MsDb$methods( getMoleculeName = function(molid) {
		stop("Method getMoleculeName() not implemented in concrete class.")
	})
	
	###############################
	# GET CHROMATOGRAPHIC COLUMNS #
	###############################
	
	# Get chromatographic columns.
	# Returns a vector of character listing the chromatographic column names. The name must be formatted in lowercase as following: uplc(-c8)?(-20min)?.
	MsDb$methods( getChromCol = function(molid = NULL) {
		stop("Method getChromCol() not implemented in concrete class.")
	})

	################
	# FIND BY NAME #
	################

	# Find a molecule by name
	# name  A vector of molecule names to search for.
	# Return an integer vector of the same size as the name input vector, containing the found molecule IDs, in the same order.
	MsDb$methods( findByName = function(name) {
		stop("Method findByName() not implemented in concrete class.")
	})
	
	#######################
	# GET RETENTION TIMES #
	#######################
	
	# Get the retention times of a molecule.
	# Returns a list of numeric vectors. The list has for keys/names the columns, and for values vectors of numerics (the retention times). If no retention times are registered for this molecule, then returns an empty list.
	MsDb$methods( getRetentionTimes = function(molid, col = NA_character_) {
		stop("Method getRetentionTimes() not implemented in concrete class.")
	})
	
	################
	# GET NB PEAKS #
	################
	
	# Get the total number of MS peaks stored inside the database.
	# molid     The ID of the molecule.
	# type      The MS type.
	MsDb$methods( getNbPeaks = function(molid = NA_integer_, type = NA_character_) {
		stop("Method getNbPeaks() not implemented in concrete class.")
	})

	##################
	# GET PEAK TABLE #
	##################

	MsDb$methods( getPeakTable = function(molid = NA_integer_, mode = NA_character_) {
		stop("Method getPeakTable() not implemented in concrete class.")
	})

	##########
	# SEARCH #
	##########

	# Find molecule MS peaks whose m/z matches the submitted m/z in the tolerance specified.
	# mode              The mode to use: either MSDB.TAG.POS or MSDB.TAG.NEG.
	# shift             The m/z shift to use, in ppm.
	# prec              The m/z precision to use, in ppm.
	# col               The chromatographic column used.
	# rt.tol            Simple retention tolerance parameter: rtinf = rt - rt.tol and rtsup = rt + rt.tol
	# rt.tol.x          Tolerance parameter for the equations : rtinf = rt - rt.tol.x - rt ^ rt.tol.y and rtsup = rt + rt.tol.x + rt ^ rt.tol.y
	# rt.tol.y          Tolerance parameter. See rt.tol.x parameter.
	# attribs           Only search for peaks whose attribution is among this set of attributions.
	# molids            Only search for peaks whose molecule ID is among this vector of integer molecule IDs. Can also be a data frame with a retention time column x.colnames$rt and a molecule ID column MSDB.TAG.MOLID.
	# molids.rt.tol     Retention time tolerance used when molids parameter is a data frame (rt, id)
	# precursor.match   Remove peaks whose molecule precursor peak has not also been matched.
	# precursor.rt.tol
	# Returns a data frame, listing m/z values provided in input. Several matches can be found for an m/z value, in which case several lines (the same number as the number of matches found) with the same m/z value repeated will be inserted. The m/z values will be listed in the same order as in the input. The columns of the data.frame are: mz, rt (only if present in the input), id, mztheo, col, colrt, composition, attribution.
	MsDb$methods( searchForMzRtList = function(x = NULL, mode, shift = NULL, prec = NULL, col = NULL, rt.tol = NULL, rt.tol.x = NULL, rt.tol.y = NULL, molids = NULL, molids.rt.tol = NULL, attribs = NULL, precursor.match = FALSE, precursor.rt.tol = NULL, same.cols = FALSE, same.rows = FALSE, peak.table = FALSE) {

		# Use provided data frame
		old.input <- NULL
		tmp.output <- NULL
		if ( ! is.null(x)) {
			tmp.input <- MsDbInputDataFrameStream$new(df = x)
			tmp.output <- MsDbOutputDataFrameStream$new()
			old.input <- .self$setInputStream(tmp.input)
			.self$addOutputStreams(tmp.output)
		}

		if (precursor.match) {
			# Get IDs of all molecules whose precursor peak matches one of the mz in the list
			precursors.df <- .self$.doSearchForMzRtList(mode = mode, shift = shift, prec = prec, col = col, rt.tol = rt.tol, rt.tol.x = rt.tol.x, rt.tol.y = rt.tol.y, attribs = .self$.prec[[mode]], output.to.stream = FALSE)
			cols.to.keep <- if (is.null(col)) MSDB.TAG.MOLID else c(MSDB.TAG.MOLID, MSDB.TAG.COL, MSDB.TAG.COLRT)
			precursors.ids <- precursors.df[, cols.to.keep, drop = FALSE]
			precursors.ids <- precursors.ids[ ! is.na(precursors.ids[[MSDB.TAG.MOLID]]), , drop = FALSE]
			precursors.ids <- precursors.ids[ ! duplicated(precursors.ids), ]

			# Get all matching peaks whose molecule is inside the previously obtained list of molecules
			df <- .self$.doSearchForMzRtList(mode = mode, shift = shift, prec = prec, col = col, rt.tol = NULL, rt.tol.x = NULL, rt.tol.y = NULL, molids = precursors.ids, molids.rt.tol = precursor.rt.tol, same.cols = same.cols, same.rows = same.rows, peak.table = peak.table)
# TODO 
#
#			peaks <- if (peak.table) results[['peaks']] else results
#
#			# Merge results with the column/rt found for precursors.
#			if ( ! is.null(col) && ! is.null(peaks)) {
#				precursors.ids <- precursors.df[, c(MSDB.TAG.MOLID, MSDB.TAG.col, MSDB.TAG.COLRT)]
#				precursors.ids <- precursors.ids[ ! is.na(precursors.ids[[MSDB.TAG.MOLID]]), ]
#
#				# Get rows where ID is NA
#				peaks.na <- peaks[is.na(peaks[[MSDB.TAG.MOLID]]), ]
#
#				# Get rows where ID is found (i.e.: not NA)
#				peaks <- peaks[, !(colnames(peaks) %in% c(MSDB.TAG.COL, MSDB.TAG.COLRT))] # drop col and colrt columns
#				peaks.not.na <- peaks[! is.na(peaks[[MSDB.TAG.MOLID]]), ]
#
#				# Add col and colrt values to found peaks
#				peaks <- merge(peaks.not.na, precursors.ids, by = MSDB.TAG.MOLID)
#
#				# Put back unfound peaks
#				peaks <- rbind(peaks, peaks.na)
#
#				# Sort
#				peaks <- peaks[order(peaks[[x.colnames$mz]], peaks[[x.colnames$rt]], peaks[[MSDB.TAG.MOLID]], peaks[[MSDB.TAG.COL]]), ]
#
#				# Remove rownames
#				rownames(peaks) <- NULL
#
#				# Reorder columns
#				peaks <- peaks[unlist(.self$.output.fields[names(.PEAK.TABLE.COLS)])]
#			}
#
#			# Remove duplicates
#			if ( ! is.null(peaks))
#				peaks <- peaks[ ! duplicated(peaks), ]
#
#			if (peak.table)
#				results[['peaks']] <- peaks
#			else
#				results <- peaks
#
#			return(results)
		}
		else
			.self$.doSearchForMzRtList(mode = mode, shift = shift, prec = prec, col = col, rt.tol = rt.tol, rt.tol.x = rt.tol.x, rt.tol.y = rt.tol.y, molids = molids, molids.rt.tol = molids.rt.tol, attribs = attribs, same.cols = same.cols, same.rows = same.rows, peak.table = peak.table)

		if ( ! is.null(x)) {
			results <- tmp.output$getDataFrame()
			.self$removeOutputStreams(tmp.output)
			.self$setInputStream(old.input)
			return(results)
		}
	})

	MsDb$methods( .doSearchForMzRtList = function(mode, shift = NULL, prec = NULL, col = NULL, rt.tol = NULL, rt.tol.x = NULL, rt.tol.y = NULL, molids = NULL, molids.rt.tol = NULL, attribs = NULL, same.cols = FALSE, same.rows = FALSE, peak.table = FALSE, output.to.stream = TRUE) {

#		# Choose columns to keep from x
#		x.cols <- if (same.cols) colnames(x) else intersect(if (is.null(col)) c(x.colnames$mz) else c(x.colnames$mz, x.colnames$rt), colnames(x))
#
#		# Create a peak fake data frame for defining columns
#		peaks.fake <- data.frame(stringsAsFactors = FALSE)
#		for (field in names(.PEAK.TABLE.COLS))
#			if ( ! is.null(col) || ! field %in% .RT.MATCHING.COLS)
#				peaks.fake[.self$.output.fields[[field]]] <- vector(mode = .PEAK.TABLE.COLS[[field]], length = 0)
#
#		# Initialize y data frame, so when x contains no rows an empty y data frame is returned with all the columns set with right type.
#		if (same.rows) {
#			y <- peaks.fake[, if (is.null(col)) c(MSDB.TAG.MZ) else c(MSDB.TAG.MZ, MSDB.TAG.RT), drop = FALSE]
#			y[MSDB.TAG.MSMATCHING] <- character()
#		}
#		else
#			y <- peaks.fake
#		y <- cbind(y, x[NULL, ! x.cols %in% colnames(y), drop = FALSE])
#		if (peak.table) {
#			z <- peaks.fake
#			z <- cbind(z, x[NULL, ! x.cols %in% colnames(z), drop = FALSE])
#		}

		# Loop on all lines of input
		peaks <- NULL
		.self$.input.stream$reset()
		while (.self$.input.stream$hasNextValues()) {

			.self$.input.stream$nextValues()

				# Search for m/z
				results <- .self$searchForMzRtTols(mode = mode, mz = .self$.input.stream$getMz(), shift = shift, prec = prec, rt = .self$.input.stream$getRt(), col = col, rt.tol = rt.tol, rt.tol.x = rt.tol.x, rt.tol.y = rt.tol.y, attribs = attribs, molids = molids, molids.rt.tol = molids.rt.tol)

				# Call output streams
				if (output.to.stream && ! is.null(.self$.output.streams))
					for (s in .self$.output.streams)
						s$matchedPeaks(mz = .self$.input.stream$getMz(), rt = if (is.null(col)) NULL else .self$.input.stream$getRt(), peaks = results, unused = .self$.input.stream$getAll(but = if (is.null(col)) c(MSDB.TAG.MZ) else c(MSDB.TAG.MZ, MSDB.TAG.RT)))

				# Append to peak list
				peaks <- rbind(peaks, results)

#				# Add results to output
#				r <- nrow(y) + 1
#				x.lines <- x[i, x.cols, drop = FALSE]
#				x.lines <- rename.col(x.lines, unlist(x.colnames), unlist(.self$.output.fields[names(x.colnames)]))
#				if (nrow(results) == 0) {
#					y[r, colnames(x.lines)] <- x.lines
#				}
#				else {
#					if (same.rows) {
#						y[r, colnames(x.lines)] <- x.lines
#						ids <- results[[MSDB.TAG.MOLID]]
#						ids <- ids[ ! duplicated(ids)] # Remove duplicated values
#						y[r, MSDB.TAG.msmatching] <- paste(ids, collapse = .self$.molids.sep)
#					}
#					if ( ! same.rows || peak.table) {
#						new.rows <- cbind(x.lines, results, row.names = NULL)
#						if ( ! same.rows) {
#							rows <- r:(r+nrow(results)-1)
#							y[rows, colnames(new.rows)] <- new.rows
#						}
#						if (peak.table) {
#							zr <- nrow(z) + 1
#							zrows <- zr:(zr+nrow(results)-1)
#							z[zrows, colnames(new.rows)] <- new.rows
#						}
#					}
#				}
		}

#		results <- if (peak.table) list(main = y, peaks = z) else y

#		return(results)
		return(peaks)
	})

	# rt        Retention time in seconds.
	# molids    An option vector of molecule IDs, used to restrict the search.
	MsDb$methods( searchForMzRtTols = function(mode, mz, rt = NULL, shift = NULL, prec = NULL, col = NULL, rt.tol = NULL, rt.tol.x = NULL, rt.tol.y = NULL, attribs = NULL, molids = NULL, molids.rt.tol = NULL, colnames = MSDB.DFT.INPUT.FIELDS) {

		# Set M/Z bounds
		if (.self$.mz.tol.unit == MSDB.MZTOLUNIT.PPM) {
			mz.low <- mz * (1 + (- shift - prec) * 1e-6)
			mz.high <- mz * (1 + (- shift + prec) * 1e-6)
		}
		else { # PLAIN
			mz.low <- mz - shift - prec
			mz.high <- mz - shift + prec
		}

		# Set retention time bounds
		rt.low <- NULL
		rt.high <- NULL
		if ( ! is.null(rt.tol)) {
			low <- rt - rt.tol
			high <- rt + rt.tol
			rt.low <- if (is.null(rt.low)) low else max(low, rt.low)
			rt.high <- if (is.null(rt.high)) high else min(high, rt.high)
		}
		if ( ! is.null(rt.tol.x)) {
			low <- rt - rt.tol.x - rt ^ rt.tol.y
			high <- rt + rt.tol.x + rt ^ rt.tol.y
			rt.low <- if (is.null(rt.low)) low else max(low, rt.low)
			rt.high <- if (is.null(rt.high)) high else min(high, rt.high)
		}

		# List molecule IDs
		if ( ! is.null(molids.rt.tol) && is.data.frame(molids)) {
			ids <- molids[(rt >= molids[[MSDB.TAG.COLRT]] - molids.rt.tol) & (rt <= molids[[MSDB.TAG.COLRT]] + molids.rt.tol), MSDB.TAG.MOLID]
			if (length(ids) == 0)
				# No molecule ID match for this retention time
				return(data.frame()) # return empty result set
		} else {
			ids <- molids
		}

		return(.self$searchForMzRtBounds(mode,
										 mz.low = mz * (1 + (- shift - prec) * 1e-6),
										 mz.high = mz * (1 + (- shift + prec) * 1e-6),
										 rt.low = rt.low,
										 rt.high = rt.high,
										 col = col,
										 attribs = attribs,
										 molids = ids))
	})

	# rt.low    Lower bound of the retention time in seconds.
	# rt.high   Higher bound of the retention time in seconds.
	MsDb$methods( searchForMzRtBounds = function(mode, mz.low, mz.high, rt.low = NULL, rt.high = NULL, col = NULL, attribs = NULL, molids = NULL) {

		results <- .self$.do.search.for.mz.rt.bounds(mode = mode, mz.low = mz.low, mz.high = mz.high, rt.low = rt.low, rt.high = rt.high, col = col, attribs = attribs, molids = molids)

		return(results)
	})

	# TODO Write description of output: data frame with which columns ?
	MsDb$methods( .do.search.for.mz.rt.bounds = function(mode, mz.low, mz.high, rt.low = NULL, rt.high = NULL, col = NULL, attribs = NULL, molids = NULL) {
		stop("Method .do.search.for.mz.rt.bounds() not implemented in concrete class.")
	})

	# DEPRECATED
	MsDb$methods( searchForMz = function(x, mode, tol = 5, col = NULL, rt.tol.x = 5, rt.tol.y = 0.80) {
		warning("Method searchForMz() is deprecated. Use searchForMzRtList() instead.")
		.self$searchForMzRtList(x = x, mode = mode, prec = tol, col = col, rt.tol.x = rt.tol.x, rt.tol.y = rt.tol.y)
	})

} # end of load safe guard
