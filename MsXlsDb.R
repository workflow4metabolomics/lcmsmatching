if ( ! exists('MsXlsDb')) { # Do not load again if already loaded

	library('methods')
	library('stringr')
	source('msdb-common.R')
	source('MsDb.R')
	source('strhlp.R', chdir = TRUE)
	source('dfhlp.R', chdir = TRUE)
	source('search.R', chdir = TRUE)
	source('excelhlp.R', chdir = TRUE)
	
	#############
	# CONSTANTS #
	#############
	
	.THIS.FILE.PATH <- getwd() # We suppose that the file has been sourced with the option chdir = TRUE

	.XLS_PEAKS_ROW_OFFSET <- 8
	.XLS_PEAKS_RT_COL_START <- 11
	.XLS_MSPOS_TAB <- 'MS_POS'
	.XLS_MSNEG_TAB <- 'MS_NEG'
	.XLS_MZ_COL <- 1
	.XLS_INTENSITY_COL <- 2
	.XLS_RELATIVE_COL <- 3
	.XLS_THEORETICAL_MZ_COL <- 5
	.XLS_COMPOSITION_COL <- 8
	.XLS_ATTRIBUTION_COL <- 9
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	MsXlsDb <- setRefClass("MsXlsDb", contains = "MsDb", fields = list(.mz.index = "ANY", .name_index = "ANY", .db_dir = "character", .limit = "numeric", .files = "ANY", .cache_dir = "character", .db = "ANY"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	MsXlsDb$methods( initialize = function(db_dir = NA_character_, limit = NA_integer_, cache_dir = NA_character_, cache = FALSE, ...) {

		# Initialize members
		                # TODO check that db_dir is not null neither na, and tests that it exists and is a directory.
		.db_dir <<- if ( ! is.null(db_dir)) db_dir else NA_character_
		.limit <<- if ( ! is.null(limit) && ! is.na(limit) && limit > 0) limit else NA_integer_
		cache_dir <- if (cache && is.na(cache_dir) && ! is.na(db_dir)) file.path(db_dir, 'cache') else cache_dir
		.cache_dir <<- if ( cache || ! is.null(cache_dir)) cache_dir else NA_character_
		.files <<- NULL
		.db <<- NULL
		.mz.index <<- NULL
		.name_index <<- NULL
	
		callSuper(...)
	})
	
	####################
	# GET MOLECULE IDS #
	####################
	
	MsXlsDb$methods( getMoleculeIds = function(max.results = NA_integer_) {
	
		# Init file list
		.self$.init.file.list()

		# Get IDs
		mol.ids <- as.integer(which( ! is.na(.self$.files)))

		# Cut
		if ( ! is.na(max.results) && length(mol.ids) > max.results)
			mol.ids <- mol.ids[max.results, ]

		return(mol.ids)
	})
	
	####################
	# GET NB MOLECULES #
	####################
	
	# Returns a list of all molecule names
	MsXlsDb$methods( getNbMolecules = function() {
		return(length(.self$getMoleculeIds()))
	})

	#####################
	# GET MOLECULE NAME #
	#####################
	
	MsXlsDb$methods( getMoleculeName = function(molid) {
		return(vapply(molid, function(m) .self$.get.mol.name(m), FUN.VALUE = ""))
	})
	
	###############################
	# GET CHROMATOGRAPHIC COLUMNS #
	###############################
	
	# Returns a list of all chromatographic columns used
	MsXlsDb$methods( getChromCol = function(molid = NULL) {
	
	    cn <- character()

		# If no molecule IDs provided, then look at all molecules
	    if	(is.null(molid))
	    	molid <- .self$getMoleculeIds()

		# Loop on molecules
		for (mid in molid) {

	    	rt <- .self$getRetentionTimes(mid)

			if ( ! is.null(rt))
				cn <- c(cn, names(rt))
		}
	
		# Remove duplicates
		cn <- cn[ ! duplicated(cn)]

		# Make data frame
		cn <- data.frame(id = cn, title = cn, stringsAsFactors = FALSE)

		return(cn)
	})

	################
	# FIND BY NAME #
	################

	MsXlsDb$methods( findByName = function(name) {

		# NULL entry
		if (is.null(name))
			return(NA_integer_)
	
		# Initialize output list
		ids <- NULL

		for (n in name) {

			id <- NA_integer_

			if ( ! is.na(n)) {

				# Get index
				index <- .self$.get.name.index()

				# Search for name in index
				i <- binary.search(toupper(n), index[['name']])

				id <- if (is.na(i)) NA_integer_ else index[i, 'id']
			}

			ids <- c(ids, id)
		}

		return(ids)
	})
	
	#######################
	# GET RETENTION TIMES #
	#######################
	
	MsXlsDb$methods( getRetentionTimes = function(molid, col = NA_character_) {

		if (is.null(molid) || is.na(molid))
			return(NULL)

		# Find it in memory
		rt <- .self$.mem.get(molid, 'rt')

		if (is.null(rt)) {
	
			# Call observers
			if ( ! is.null(.self$.observers))
				for (obs in .self$.observers)
					obs$progress(paste0("Loading retention times of file", .self$.get.file(molid), "."), level = 2)
		
			rt <- NULL
		
			# Load from cache file
			cache_file <- NA_character_
			if ( ! is.na(.self$.get.cache.dir())) {
				cache_file <- file.path(.self$.get.cache.dir(), paste0('rt-', molid, '.bin'))
				if (file.exists(cache_file))
					load(file = cache_file) # load rt
			}
		
			if (is.null(rt)) {
		
				# Get retention times of both positive and negative mode tabs
				mspos_rt <- .self$.parse_retention_times(molid, .XLS_MSPOS_TAB)
				msneg_rt <- .self$.parse_retention_times(molid, .XLS_MSNEG_TAB)
		
				# Retention times stored in negative and positive modes
				if ( ! is.null(mspos_rt) && ! is.null(msneg_rt)) {

					# Warn observers when both retention time lists are not identical
					if ( ! identical(mspos_rt, msneg_rt))
						for (obs in .self$.observers)
							obs$warning(paste0("Retention times in negative and positive modes are different in file ", .self$.get.file(molid), "."))
		
					# Merge both lists
					rt <- mspos_rt
					for (c in names(msneg_rt))
						if (c %in% names(rt)) {
							v <- c(rt[[c]], msneg_rt[[c]])
							rt[[c]] <- v[ ! duplicated(v)]
						}
						else
							rt[[c]] <- msneg_rt[[c]]
				}
				else
					# Set retention times
					rt <- if (is.null(mspos_rt)) msneg_rt else mspos_rt

				if (is.null(rt)) rt <- list()
		
				# Write in cache
				if ( ! is.na(cache_file)) {
		
					# Call observers
					if ( ! is.null(.self$.observers))
						for (obs in .self$.observers)
							obs$progress(paste0("Caching retention times of file ", .self$.get.file(molid), "."))

					save(rt, file = cache_file)
				}
			}

			# Store in memory
			.self$.mem.set(rt, molid, 'rt')
		}

		# Select only one column if asked
		if ( ! is.na(col)) rt <- rt[[col]]

		return(rt)
	})

	#################
	# GET NB PEAKS #
	#################
	
	MsXlsDb$methods( getNbPeaks = function(molid = NA_integer_, type = NA_character_) {

		# Initialize parameters
		if (is.null(molid) || (length(molid) == 1 && is.na(molid)))
			molid <- .self$getMoleculeIds()
		if (is.na(type))
			type <- c(MSDB.TAG.POS, MSDB.TAG.NEG)

		return(sum(vapply(molid, function(m) { if (is.na(m)) 0 else sum(vapply(type, function(t) { peaks <- .self$.get.peaks(m, t) ; if (is.null(peaks)) 0 else nrow(peaks) }, FUN.VALUE = 1)) }, FUN.VALUE = 1)))
	})

	##################
	# GET PEAK TABLE #
	##################

	MsXlsDb$methods( getPeakTable = function(molid = NA_integer_, mode = NA_character_) {

		peaks <- NULL

		# Set default molecule IDs
		if (is.null(molid) || (length(molid) == 1 && is.na(molid)))
			molid <- .self$getMoleculeIds()

		# Set default modes
		if (is.null(mode) || (length(mode) == 1 && is.na(mode)))
			mode <- c(MSDB.TAG.POS, MSDB.TAG.NEG)

		# Loop on all molecules
		for (mol in molid) {

			# Loop on all modes
			for (mod in mode) {
				m.peaks <- .self$.get.peaks(mol, mod)
				if ( ! is.null(m.peaks) && nrow(m.peaks) > 0) {
					m.peaks[[MSDB.TAG.MOLID]] <- mol
					m.peaks[[MSDB.TAG.MODE]] <- mod
					peaks <- if (is.null(peaks)) m.peaks else rbind(peaks, m.peaks)
					peaks <- df.move.col.first(peaks, c(MSDB.TAG.MOLID, MSDB.TAG.MODE))
				}
			}
		}

		return(peaks)
	})
	
	#################
	# GET MZ VALUES #
	#################
	
	# Returns a numeric vector of all masses stored inside the database.
	MsXlsDb$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {

		mz <- numeric()

		# Get all mz values of all molecules
		for(molid in .self$getMoleculeIds())
			for (m in (if (is.null(mode) || is.na(mode)) c(MSDB.TAG.POS, MSDB.TAG.NEG) else mode))
				mz <- c(mz, .self$.get.peaks(molid, m)[[MSDB.TAG.MZTHEO]])

		# Remove duplicated
		mz <- mz[ ! duplicated(mz)]

		# Apply cut-off
		if ( ! is.na(max.results))
			mz <- mz[1:max.results]

		return(mz)
	})
	
	#############
	# GET PEAKS #
	#############
	
	MsXlsDb$methods( .get.peaks = function(molid, mode) {
	
		tab <- if (mode == MSDB.TAG.POS) .XLS_MSPOS_TAB else .XLS_MSNEG_TAB

		# Find it in memory
		peak_df <- .self$.mem.get(molid, 'peaks', mode)

		if (is.null(peak_df)) {
			# Call observers
			if ( ! is.null(.self$.observers))
				for (obs in .self$.observers)
					obs$progress(paste0("Loading peaks of tab ", tab, " of file ", .self$.get.file(molid), "."), level = 2)
		
			peak_df <- NULL
		
			# Load from cache file
			cache_file <- NA_character_
			if ( ! is.na(.self$.get.cache.dir())) {
				cache_file <- file.path(.self$.get.cache.dir(), paste0('peaks-', molid, '-', tab, '.csv'))
				if (file.exists(cache_file))
					peak_df <- read.csv(cache_file, header = TRUE, stringsAsFactors = FALSE)
			}
		
			# Read from XLS file, if not in cache
			if (is.null(peak_df)) {
		
				# Load tab (peaks start at row 8)
				if (.self$.tab.exists(.self$.get.file(molid), tab)) {

					peaks <- read.excel(.self$.get.file(molid), tab, start.row = .XLS_PEAKS_ROW_OFFSET, stringsAsFactors = FALSE)
					if ( ! is.null(peaks))
						peaks <- peaks[ ! is.na(peaks[.XLS_MZ_COL]), , drop = FALSE] # Remove rows where m/z is not defined. TODO maybe call observer for notify a line with non NA values but without m/z value.
		
					# Instantiate peaks
					if ( ! is.null(peaks) && nrow(peaks) > 0) {
						peak_df <- peaks[1:length(peaks[[.XLS_MZ_COL]]), c(.XLS_MZ_COL, .XLS_THEORETICAL_MZ_COL, .XLS_INTENSITY_COL, .XLS_RELATIVE_COL, .XLS_COMPOSITION_COL, .XLS_ATTRIBUTION_COL), drop = FALSE]
						colnames(peak_df) <- c(MSDB.TAG.MZEXP, MSDB.TAG.MZTHEO, MSDB.TAG.INT, MSDB.TAG.REL, MSDB.TAG.COMP, MSDB.TAG.ATTR)
					}
		
					# Set default data frame (important for cache file writing, because we need a correct header to be written in order for loading)
					else {
						peak_df <- data.frame(stringsAsFactors = FALSE)
						peak_df[MSDB.TAG.MZEXP] <- numeric()
 				   		peak_df[MSDB.TAG.MZTHEO] <- numeric()
 				   		peak_df[MSDB.TAG.INT] <- numeric()
 				   		peak_df[MSDB.TAG.REL] <- numeric()
 				   		peak_df[MSDB.TAG.COMP] <- character()
 				   		peak_df[MSDB.TAG.ATTR] <- character()
					}

					if (is.null(peak_df)) peak_df <- data.frame()
		
					# Write in cache
					if ( ! is.na(cache_file)) {
		
						# Call observers
						if ( ! is.null(.self$.observers))
							for (obs in .self$.observers)
								obs$progress(paste0("Caching peaks of tab ", tab, " of file ", .self$.get.file(molid), "."))
			
						write.csv(peak_df, cache_file, row.names = FALSE)
					}
				}
			}

			# Store in memory
			.self$.mem.set(peak_df, molid, 'peaks', mode)
		}

		return(peak_df)
	})
	
	##############################
	# GET FULL MS PEAK M/Z INDEX #
	##############################
	
	# Get mz index for full ions, creating it if necessary.
	MsXlsDb$methods( .get.mz.index = function(mode) {
	
		if (is.null(.self$.mz.index[[mode]])) {
	
			# Initialize data frame
			mzi <- data.frame(stringsAsFactors = FALSE)
			mzi[MSDB.TAG.MZTHEO]    <- numeric()
			mzi[MSDB.TAG.MOLID]     <- character()
			mzi[MSDB.TAG.COMP]      <- character()
			mzi[MSDB.TAG.ATTR]      <- character()

			# Loop on all molecules
			for(molid in .self$getMoleculeIds()) {

				# Get all peaks of this molecule
				peaks <- .self$.get.peaks(molid, mode)

				# Remove rows whose mz is NA.
				peaks <- peaks[ ! is.na(peaks[[MSDB.TAG.MZTHEO]]), ]

				if (nrow(peaks) > 0) {

					# Add id column
					peaks[MSDB.TAG.MOLID] <- molid

					# Append peaks
					r <- nrow(mzi) + 1
					rows <- r:(r+nrow(peaks)-1)
					mzi[rows, ] <- peaks[colnames(mzi)]
				}
			}
	
			# Sort by M/Z
			sorted_indices <- order(mzi[[MSDB.TAG.MZTHEO]])
	
			# Group in a data frame
			.self$.mz.index[[mode]] <- mzi[sorted_indices, ]
		}

		return(.self$.mz.index[[mode]])
	})

	######################
	# SEARCH FOR MZ & RT #
	######################

	MsXlsDb$methods( .do.search.for.mz.rt.bounds = function(mode, mz.low, mz.high, rt.low = NULL, rt.high = NULL, col = NULL, attribs = NULL, molids = NULL) {

		# Search for m/z
		results <- .self$.do.search.for.mz(mode, mz.low, mz.high)

		# Filter on attributions
		if ( ! is.null(attribs)) {
			results <- results[results[[MSDB.TAG.ATTR]] %in% attribs, ]
		}

		# Filer on molecule IDs
		if ( ! is.null(molids)) {
			results <- results[results[[MSDB.TAG.MOLID]] %in% molids, ]
		}

		# Use retention time
		if ( ! is.null(col) && ! is.null(rt.low) && ! is.null(rt.high)) {

			# Get list of unique IDs
			ids <- results[[MSDB.TAG.MOLID]]
			ids <- ids[ ! duplicated(ids)]
			rt <- .self$.search.for.rt(mols = ids, rt.low = rt.low, rt.high = rt.high, col = col)
			results <- results[results[[MSDB.TAG.MOLID]] %in% rt[[MSDB.TAG.MOLID]], ]
			results <- merge(results, rt)
		}

		return(results)
	})

	##############################
	# SEARCH FOR M/Z IN MS PEAKS #
	##############################

	MsXlsDb$methods( .do.search.for.mz = function(mode, mz.low, mz.high) {
	
		results <- data.frame(stringsAsFactors = FALSE)
		results[MSDB.TAG.MZTHEO]    <- numeric()
		results[MSDB.TAG.MOLID]     <- character()
		results[MSDB.TAG.MOLNAMES]  <- character()
		results[MSDB.TAG.COMP]      <- character()
		results[MSDB.TAG.ATTR]      <- character()

		# Create m/z index
		mz_index <- .self$.get.mz.index(mode)
	
		# Find molecules
		low_bound <- binary.search(mz.low, mz_index[[MSDB.TAG.MZTHEO]], lower = FALSE)
		high_bound <- binary.search(mz.high, mz_index[[MSDB.TAG.MZTHEO]], lower = TRUE)

		# Get results
		if ( ! is.na(high_bound) && ! is.na(low_bound) && low_bound <= high_bound)
			results <- mz_index[low_bound:high_bound,]

		# Remove row names
		rownames(results) <- NULL

		return(results)
	})

	################
	# GET MOL NAME #
	################
	
	MsXlsDb$methods( .get.mol.name = function(molid) {

		if (is.na(molid))
			return(NA_character_)

		# Find it in memory
		name <- .self$.mem.get(molid, 'name')

		if (is.null(name)) {

			# Load molecule
			mol <- .self$.load.molecule(molid)
			
			# Look for name in tabs
			for (tab in c(.XLS_MSPOS_TAB, .XLS_MSNEG_TAB)) {
				hdr <- mol[[tab]][['header']]
				if ( ! is.null(hdr))
					name <- hdr[[1]]
				if ( ! is.null(name) && ! is.na(name)) break
			}

			# Store in memory
			if (is.null(name)) name <-  NA_character_
			.self$.mem.set(name, molid, 'name')
		}

		return(name)
	})
	
	##################
	# GET NAME INDEX #
	##################
	
	# Get name index.
	MsXlsDb$methods( .get.name.index = function() {
	
		if (is.null(.self$.name_index)) {
	
			# Get names
			names <- vapply(.self$getMoleculeIds(), function(id) toupper(.self$getMoleculeName(id)), FUN.VALUE = "")
	
			# Get molecule IDs
			id <- .self$getMoleculeIds()
	
			# Sort by names
			sorted_indices <- order(names)
	
			# Group in a data frame
			.self$.name_index <- data.frame(name = rbind(names)[, sorted_indices],
			                                id = rbind(id)[, sorted_indices],
			                                stringsAsFactors = FALSE)
		}

		return(.self$.name_index)
	})

	##################
	# INIT FILE LIST #
	##################
	
	MsXlsDb$methods( .init.file.list = function() {

		if (is.null(.self$.files)) {
	
			# List all files
			files <- Sys.glob(file.path(.self$.db_dir, '*.xls')) 

			# Limit the size of the database
			if ( ! is.na(.self$.limit))
				files <- head(files, .self$.limit)
	
			# Get IDs
			ids <- vapply(files, function(f) .extract_molecule_id_from_filename(f), FUN.VALUE = 1)

			# Use ids as indices to build the vector of files
			.files <<- rep(NA_character_, max(ids))
			.files[ids] <<- files
		}
	})

	#################
	# GET CACHE DIR #
	#################
	
	MsXlsDb$methods( .get.cache.dir = function() {
	
		if ( ! is.na(.self$.cache_dir) && ! file.exists(.self$.cache_dir))
			dir.create(.self$.cache_dir)
	
		return(.self$.cache_dir)
	})

	#################
	# LOAD MOLECULE #
	#################
	
	MsXlsDb$methods( .load.molecule = function(molid) {

		# Init local variables
		mol <- NULL
		cache_file <- NA_character_
		excel_file <- .self$.get.file(molid)

		# Call observers
		if ( ! is.null(.self$.observers))
			for (obs in .self$.observers)
				obs$progress(paste0("Loading molecule ", molid, "."), level = 2)

		# Load from cache
		if ( ! is.na(.self$.get.cache.dir())) {
			cache_file <- file.path(.self$.get.cache.dir(), paste0(molid, '.bin'))
			if (file.exists(cache_file))
				load(file = cache_file) # load mol variable
		}

		# Load from Excel file & write to cache
		if (is.null(mol) && ! is.na(excel_file)) {

			source(file.path(.THIS.FILE.PATH, 'excelhlp.R'), chdir = TRUE) # we use the path set when sourcing the file, since when calling this method, the current path could be different.

			# Load from Excel file
			for(tab in c(.XLS_MSPOS_TAB, .XLS_MSNEG_TAB)) {

				# Test that tab exists
				if (.self$.tab.exists(excel_file, tab)) {
					header <- read.excel(excel_file, tab, start.row = 1, end.row = .XLS_PEAKS_ROW_OFFSET - 1, header = FALSE, stringsAsFactors = FALSE, trim.values = TRUE, col.index = c(1))[[1]]
					peaks <- read.excel(excel_file, tab, start.row = .XLS_PEAKS_ROW_OFFSET)
					mol[[tab]] <- list(header = header, peaks = peaks)
				}

				# Missing tab
				else {
					for (obs in .self$.observers)
						obs$warning(paste0("No excel tab ", tab, " in file ", excel_file, "."))
				}
			}
	
			# Write in cache
			if ( ! is.na(cache_file)) {
	
				# Call observers
				if ( ! is.null(.self$.observers))
					for (obs in .self$.observers)
						obs$progress(paste0("Caching file ", excel_file, "."))

				save(mol, file = cache_file)
			}
		}

		return(mol)
	})
	
	########################
	# DOES EXCEL TAB EXIST #
	########################
	
	MsXlsDb$methods( .tab.exists = function(file, tab) {
	
		source(file.path(.THIS.FILE.PATH, 'excelhlp.R'), chdir = TRUE) # we use the path set when sourcing the file, since when calling this method, the current path could be different.
	
		if ( ! tab.exists(file, tab)) {

			# Warn observers
			for (obs in .self$.observers)
				obs$warning(paste0("No excel tab ", tab, " in file ", file, "."))

			return(FALSE)
		}
	
		return(TRUE)
	})
	
	#########################
	# PARSE RETENTION TIMES #
	#########################
	
	MsXlsDb$methods( .parse_retention_times = function(id, tab) {
	
	    rt <- NULL
	
		if (.self$.tab.exists(.self$.get.file(id), tab)) {
			peaks <- read.excel(.self$.get.file(id), tab, start.row = .XLS_PEAKS_ROW_OFFSET)
	
			# Get retention times
			if ( ! is.null(peaks) && length(peaks) > 0 && ! is.na(peaks[[1]][[1]]))
				for (c in .XLS_PEAKS_RT_COL_START:length(names(peaks)))
					if ( ! is.na(peaks[[c]][[1]])) {
	
						# Check retention times of all different m/z peaks for the same column.
						.self$.check_retention_times(id, tab, names(peaks)[[c]], peaks[[c]], sum( ! is.na(peaks[[1]])))
	
						# Add retention time
						# TODO The column names are transformed through the read.xlsx call. For instance:
						#           HPLC (C18) 25mn QTOF (Bis)                  --> HPLC..C18..25mn.QTOF..Bis.
	  					#           ZICpHILIC 150*5*2.1 Shimadzu-Exactive-42mn  --> ZICpHILIC.150.5.2.1.Shimadzu.Exactive.42mn
						# This can be an issue, since we loose the formating.
						col_id <- names(peaks)[[c]]
						time <- peaks[[c]][[1]] * 60 # Read and convert retention time in seconds.
						if (is.null(rt) || ! col_id %in% names(rt))
							rt[[col_id]] <- list(time)
						else
							rt[[col_id]] <- c(rt[[col_id]], time)
					}
		}
		
		return(rt)
	})
	
	#########################
	# CHECK RETENTION TIMES #
	#########################
	
	MsXlsDb$methods( .check_retention_times = function(id, tab_name, column_name, rt, n) {
	
		if (n >= 1  && ! is.null(.self$.observers) && length(.self$.observers) > 0)
	
			# Check column only if there is at least one value inside
			if (sum( ! is.na(rt)) > 0)
	
				# Loop on all values
				for(i in 1:n) {
	
					# Check that it's defined
					if (i > 1 && is.na(rt[[i]]))
						for (obs in .self$.observers)
							obs$warning(paste0("Retention times undefined for column ", column_name, " at row ", i + .XLS_PEAKS_ROW_OFFSET, " of tab ", tab_name, " in file ", .self$.get.file(id), "."))
	
					else if (i > 1)
						# Check the value (it must be constant)
						if (rt[[i-1]] != rt[[i]])
							for (obs in .self$.observers)
								obs$error(paste0("Retention times not constant for column ", column_name, " between row ", i - 1 + .XLS_PEAKS_ROW_OFFSET, " and row ", i + .XLS_PEAKS_ROW_OFFSET, "o tab", tab_name, "in file", .self$.get.file(id)))
				}
	})
	
	####################
	# GET FILE FROM ID #
	####################
	
	MsXlsDb$methods( .get.file = function(id) {
	
		# List files
		.self$.init.file.list()
	
		return( if (id > 0 && id <= length(.self$.files)) .self$.files[id] else NA_character_)
	})
	
	###########
	# MEM GET #
	###########

	# Get database data from memory
	MsXlsDb$methods( .mem.get = function(molid, field, second.field = NA_character_) {

		data <- .self$.db[[as.character(molid)]][[field]]

		if ( ! is.na(second.field))
			data <- data[[second.field]]

		return(data)
	})
	
	###########
	# MEM SET #
	###########

	# Set database data into memory
	MsXlsDb$methods( .mem.set = function(data, molid, field, second.field = NA_character_) {

		id <- as.character(molid)

		# Create db
		if (is.null(.self$.db))
		    .db <<- list()

		# Create first level
		if (is.null(.self$.db[[id]]))
			.self$.db[[id]] <- list()

		# Create second level
		if ( ! is.na(second.field) && is.null(.self$.db[[id]][[field]]))
			.self$.db[[id]][[field]] <- list()

		# Store data
		if (is.na(second.field)) {
			.self$.db[[id]][[field]] <- data
		} else {
			.self$.db[[id]][[field]][[second.field]] <- data
		}
	})

	#################
	# SEARCH FOR RT #
	#################

	# Find molecules matching a certain retention time.
	# col           A list of chromatographic columns to use.
	# rt.low        The lower bound of the rt value.
	# rt.high       The higher bound of the rt value.
	# mols          A list of molecule IDs to process. If unset, then take all molecules.
	# Return a data frame with the following columns: id, col, colrt.
	MsXlsDb$methods( .search.for.rt = function(col, rt.low, rt.high, mols = NULL) {

		# Use all molecules if no list is provided
		if (is.null(mols))
			mols <- .self$getMoleculeIds()

		results <- data.frame(id = integer(), col = character(), colrt = double(), stringsAsFactors = FALSE)
		colnames(results) <- c(MSDB.TAG.MOLID, MSDB.TAG.COL, MSDB.TAG.COLRT)

		# Loop on all molecules
		for (molid in mols) {
			no.col <- TRUE
			for (c in col) {
				molrts <- .self$getRetentionTimes(molid, c)
				if ( ! is.null(molrts)) {
					no.col <- FALSE
					for (molrt in molrts) {
						if (molrt >= rt.low && molrt <= rt.high) {
							r <- nrow(results) + 1
							results[r, ] <- c(id = molid, col = c, colrt = molrt)
						}
					}
				}
			}

			if (no.col) {
				r <- nrow(results) + 1
				results[r, c(MSDB.TAG.MOLID)] <- c(id = molid)
			}
		}

		return(results)
	})

	############################
	# EXTRACT ID FROM FILENAME #
	############################
	
	.extract_molecule_id_from_filename <- function(filename) {
	
		id <- NA_integer_
	
		if ( ! is.na(filename)) {
			g <- str_match(filename, "N(\\d+)[._-]")
			if ( ! is.na(g[1,1]))
				id <- as.numeric(g[1,2])
		}
	
		return(id)
	}
	
} # end of load safe guard
