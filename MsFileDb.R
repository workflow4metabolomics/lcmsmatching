if ( ! exists('MsFileDb')) { # Do not load again if already loaded

	library('methods')
	source('MsDb.R')
	source('msdb-common.R')
	source('search.R', chdir = TRUE)

	#####################
	# CLASS DECLARATION #
	#####################
	
	MsFileDb <- setRefClass("MsFileDb", contains = "MsDb", fields = list(.file = "character", .db = "ANY", .fields = "list", .modes = "list", .name.to.id = "ANY"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	MsFileDb$methods( initialize = function(file = NA_character_, ...) {

		# Initialize members
		.file <<- if ( ! is.null(file)) file else NA_character_
		.db <<- NULL
		.fields <<- msdb.get.dft.db.fields()
		.modes <<- MSDB.DFT.MODES
		.name.to.id <<- NULL
	
		callSuper(...)
	})
	
	#################
	# SET DB FIELDS #
	#################
	
	MsFileDb$methods( areDbFieldsSettable = function() {
		return(TRUE)
	})
	
	MsFileDb$methods( setDbFields = function(fields) {
		.fields <<- as.list(fields)
	})
	
	################
	# CHECK FIELDS #
	################
	
	MsFileDb$methods( .check.fields = function(fields) {

		if (is.null(fields))
			stop("No fields specified for .check.fields()")

		# Check that fields are defined in the fields list
		unknown <- fields[ ! fields %in% names(.self$.fields)]
		if (length(unknown) > 0)
			stop(paste0("Database field", if (length(unknown) == 1) "" else "s", " \"", paste(unkown, collapse = ", "), "\" ", if (length(unknown) == 1) "is" else "are", " not defined."))

		# Check that field values are real columns inside the database
		.self$.init.db()
		db.col.names <- fields #vapply(fields, function(s) .self$.fields[[s]], FUN.VALUE = '')
		unknown.cols <- db.col.names[ ! db.col.names %in% colnames(.self$.db)]
		if (length(unknown.cols) > 0)
			stop(paste0("Column", if (length(unknown.cols) == 1) "" else "s", " \"", paste(unknown.cols, collapse = ", "), "\" ", if (length(unknown.cols) == 1) "is" else "are", " not defined inside the database \"", .self$.file, "\"."))
	})

	################
	# SET MS MODES #
	################
	
	MsFileDb$methods( areDbMsModesSettable = function() {
		return(TRUE)
	})
	
	MsFileDb$methods( setDbMsModes = function(modes) {
		.modes <<- as.list(modes)
	})
	
	###########
	# INIT DB #
	###########

	MsFileDb$methods( .init.db = function() {

		if (is.null(.self$.db)) {

			# Load database
			.db <<- read.table(.self$.file, sep = "\t", quote = "\"", header = TRUE, stringsAsFactors = FALSE, row.names = NULL)

			# Check that colnames are unique
			dupcol <- duplicated(colnames(.self$.db))
			if (any(dupcol))
				stop(paste("Database header contains duplicated names: ", paste(unique(colnames(.self$.db)[dupcol]), collapse = ', '), "."))

			# Check that columns names supplied through field map are unique
			dupfields <- duplicated(.self$.fields)
			if (any(dupfields))
				stop(paste("Some db column names supplied are duplicated: ", paste(unique(.self$.fields[dupfields]), collapse = ', '), "."))

			# Rename columns
			colnames(.self$.db) <- vapply(colnames(.self$.db), function(c) if (c %in% .self$.fields) names(.self$.fields)[.self$.fields %in% c] else c, FUN.VALUE = '')
		}
	})

	############
	# GET DATA #
	############

	MsFileDb$methods( .get = function(db = NULL, col = NULL) {
	
		# Init db
		if  (is.null(db)) {
			.self$.init.db()
			db <- .self$.db
		}

		# Check fields
		.self$.check.fields(col)

		# Get database columns
#		db.cols <- unlist(.self$.fields[col])

		return(db[, col])
	})

	###########
	# GET ROW #
	###########

	MsFileDb$methods( .get.row = function(row, cols = NULL) {
	
		# Init db
		.self$.init.db()

		# Check fields
		if ( ! is.null(cols))
			.self$.check.fields(cols)

		if ( ! is.null(cols)) {
			#cols <- vapply(cols, function(c) .self$.fields[[c]], FUN.VALUE = '')
			return(.self$.db[row, cols])
		}

		return(.self$.db[row, ])
	})

	###########
	# GET COL #
	###########

	MsFileDb$methods( .get.col = function(col) {
	
		# Init db
		.self$.init.db()

		# Check fields
		.self$.check.fields(col)

		#return(.self$.db[[.self$.fields[[col]]]])
		return(.self$.db[[col]])
	})

	####################
	# GET MOLECULE IDS #
	####################
	
	MsFileDb$methods( getMoleculeIds = function(max.results = NA_integer_) {
	
		# Init db
		.self$.init.db()

		# Get IDs
		mol.ids <- as.character(.self$.get.col(MSDB.TAG.MOLID))
		mol.ids <- mol.ids[ ! duplicated(mol.ids)]
		mol.ids <- sort(mol.ids)

		# Cut results
		if ( ! is.na(max.results) && length(mol.ids) > max.results)
			mol.ids <- mol.ids[1:max.results]

		return(mol.ids)
	})

	####################
	# GET NB MOLECULES #
	####################
	
	# Returns the number of molecules in the database.
	MsFileDb$methods( getNbMolecules = function() {
	
		# Init db
		.self$.init.db()

		# Get IDs
		mol.ids <- .self$.get.col(MSDB.TAG.MOLID)
		mol.ids <- mol.ids[ ! duplicated(mol.ids)]

		return(length(mol.ids))
	})
	
	#####################
	# GET MOLECULE NAME #
	#####################
	
	MsFileDb$methods( .get.name.from.id = function(db, id) {

		if(is.na(id))
			return(NA_character_)

		# Get names
		names <- db[db[[MSDB.TAG.MOLID]] %in% id, MSDB.TAG.MOLNAMES]
		if (length(names) == 0)
			return(NA_character_)

		# Each molecule has potentially several names. Since we must return only one name for each molecule, we choose the first one.
		name <- strsplit(names, ';')[[1]][[1]]

		return(name)
	})

	# Get molecule names
	# molid     An integer vector of molecule IDs.
	# Returns a character vector containing the names of the molecule IDs, in the same order as the input vector.
	MsFileDb$methods( getMoleculeName = function(molid) {

		if (is.null(molid))
			return(NA_character_)

		# Init db
		.self$.init.db()

		# Get database
		db <- .self$.db[, c(MSDB.TAG.MOLID, MSDB.TAG.MOLNAMES)]

		# Remove duplicates
		db <- db[! duplicated(db[[MSDB.TAG.MOLID]]), ]

		# Look for ids
		names <- vapply(molid, function(i) .self$.get.name.from.id(db, i), FUN.VALUE = '')

		return(names)
	})

	###################
	# INIT NAME TO ID #
	###################

	MsFileDb$methods( .init.name.to.id = function() {

		if (is.null(.self$.name.to.id)) {

			# Create data frame
			.name.to.id <<- data.frame(name = character(), id = character(), stringsAsFactors = FALSE)

			# Init db
			.self$.init.db()

			# Get database subset (columns name and id only).
			db <- .self$.db[, c(MSDB.TAG.MOLID, MSDB.TAG.MOLNAMES)]

			# Remove duplicate IDs
			db <- db[! duplicated(db[[MSDB.TAG.MOLID]]), ]

			# Loop on all 
			for(i in seq(db[[MSDB.TAG.MOLID]])) {
				i.id <- db[i, MSDB.TAG.MOLID]
				i.names <- split.str(db[i, MSDB.TAG.MOLNAMES], ';', unlist = TRUE)
				.name.to.id <<- rbind(.self$.name.to.id, data.frame(name = toupper(i.names), id = rep(i.id, length(i.names)), stringsAsFactors = FALSE))
			}

			# Order by name
			.name.to.id <<- .self$.name.to.id[order(.self$.name.to.id[['name']]), ]
		}
	})

	####################
	# GET ID FROM NAME #
	####################

	MsFileDb$methods( .get.id.from.name = function(name) {

		# Initialize name.to.id search tree
		.self$.init.name.to.id()

		# Search for name
		i <- binary.search(toupper(name), .self$.name.to.id[['name']])

		# Get ID
		id <- if (is.na(i)) NA_character_ else as.character(.self$.name.to.id[i, 'id'])

		return(id)
	})

	################
	# FIND BY NAME #
	################

	# Find a molecule by name
	# name  A vector of molecule names to search for.
	# Return a vector of the same size as the name input vector, containing the found molecule IDs, in the same order.
	MsFileDb$methods( findByName = function(name) {

		if (is.null(name))
			return(NA_character_)

		# Look for molecules with this name
		ids <- list()
		for (n in name)
			ids <- c(ids, list(.self$.get.id.from.name(n)))

		return(ids)
	})
	
	###############################
	# GET CHROMATOGRAPHIC COLUMNS #
	###############################
	
	MsFileDb$methods( getChromCol = function(molid = NULL) {

		# Init db
		.self$.init.db()

		# Get database
		db <- .self$.db[, c(MSDB.TAG.MOLID, MSDB.TAG.COL)]

		# Filter on molecule IDs
		if ( ! is.null(molid))
			db <- db[db[[MSDB.TAG.MOLID]] %in% molid,]

		# Get column names
		cols <- db[[MSDB.TAG.COL]]

		# Remove duplicates
		cols <- cols[ ! duplicated(cols)]

		# Make data frame
		cols <- data.frame(id = cols, title = cols, stringsAsFactors = FALSE)

		return(cols)
	})
	
	################
	# GET NB PEAKS #
	################
	
	# Get the total number of MS peaks stored inside the database.
	# molid     The ID of the molecule.
	# type      The MS type.
	MsFileDb$methods( getNbPeaks = function(molid = NA_integer_, type = NA_character_) {

		# Init db
		.self$.init.db()

		# Get database
		db <- .self$.db[, c(MSDB.TAG.MOLID, MSDB.TAG.MODE, MSDB.TAG.MZTHEO)]

		# Filter on mode
		if ( ! is.null(type) && ! is.na(type))
			db <- db[db[[MSDB.TAG.MODE]] == (if (type == MSDB.TAG.POS) .self$.modes$pos else .self$.modes$neg), ]

		# Filter on molecule IDs
		if ( ! is.null(molid) && ! is.na(molid))
			db <- db[db[[MSDB.TAG.MOLID]] %in% molid,]

		# Get mz values
		mz <- db[[MSDB.TAG.MZTHEO]]

		# Count number of unique values
		n <- sum(as.integer(! duplicated(mz)))

		return(n)
	})

	##########
	# SEARCH #
	##########

	MsFileDb$methods( .do.search.for.mz.rt.bounds = function(mode, mz.low, mz.high, rt.low = NULL, rt.high = NULL, col = NULL, attribs = NULL, molids = NULL) {

		# Init db
		.self$.init.db()
		db <- .self$.db

		# Filter on mode
		if ( ! is.null(mode) && ! is.na(mode))
			db <- db[db[[MSDB.TAG.MODE]] == (if (mode == MSDB.TAG.POS) .self$.modes$pos else .self$.modes$neg), ]

		# Filter on molecule IDs
		if ( ! is.null(molids))
			db <- db[db[[MSDB.TAG.MOLID]] %in% molids,]

		# Filter on attributions
		if ( ! is.null(attribs) && ! is.na(attribs))
			db <- db[db[[MSDB.TAG.ATTR]] %in% attribs,]

		# Filter on columns
		if ( ! is.null(col) && ! is.na(col))
			db <- db[db[[MSDB.TAG.COL]] %in% col,]

		# Filter on retention time
		if ( ! is.null(rt.low) && ! is.na(rt.low) && ! is.null(rt.high) && ! is.na(rt.high)) {
			scale <- if (.self$getRtUnit() == MSDB.RTUNIT.MIN) 60 else 1
			db <- db[db[[MSDB.TAG.COLRT]] * scale >= rt.low & db[[MSDB.TAG.COLRT]] * scale <= rt.high, ]
		}

		# Remove retention times and column information
		if (is.null(col) || is.na(col) || is.null(rt.low) || is.na(rt.low) || is.null(rt.high) || is.na(rt.high)) {
			db <- db[, ! (colnames(db) %in% c(MSDB.TAG.COL, MSDB.TAG.COLRT))]

			# Remove duplicates
			db <- db[ ! duplicated(db), ]
		}

		# Filter on mz
		db <- db[db[[MSDB.TAG.MZTHEO]] >= mz.low & db[[MSDB.TAG.MZTHEO]] <= mz.high, ]

		return(db)
	})
	
	#################
	# GET MZ VALUES #
	#################
	
	# Returns a numeric vector of all masses stored inside the database.
	MsFileDb$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {

		# Init db
		.self$.init.db()
		db <- .self$.db

		# Filter on mode
		if ( ! is.null(mode) && ! is.na(mode)) {
			mode.tag <- if (mode == MSDB.TAG.POS) .self$.modes$pos else .self$.modes$neg
			selected.lines <- (.self$.get(db, col = MSDB.TAG.MODE) == mode.tag)
			db <- db[selected.lines, ]
		}

		# Get masses
		mz <- .self$.get(db, col = MSDB.TAG.MZTHEO)

		# Remove duplicates
		mz <- mz[ ! duplicated(mz)]

		# Apply cut-off
		if ( ! is.na(max.results))
			mz <- mz[1:max.results]

		return(mz)
	})
	
	#######################
	# GET RETENTION TIMES #
	#######################
	
	# Get the retention times of a molecule.
	# Returns a list of numeric vectors. The list has for keys/names the columns, and for values vectors of numerics (the retention times). If no retention times are registered for this molecule, then returns an empty list.
	MsFileDb$methods( getRetentionTimes = function(molid, col = NA_character_) {

		if (is.null(molid) || is.na(molid))
			return(list())

		# Init db
		.self$.init.db()
		db <- .self$.db[, c(MSDB.TAG.MOLID, MSDB.TAG.COL, MSDB.TAG.COLRT)]

		# Filter on molecule ID
		if ( ! is.null(molid) && ! is.na(molid))
			db <- db[db[[MSDB.TAG.MOLID]] %in% molid,]

		# Remove duplicates
		db <- db[! duplicated(db), ]

		# Build retention time list
		rt <- list()
		cols <- db[[MSDB.TAG.COL]]
		cols <- cols[ ! duplicated(cols)]
		for (col in cols) {
			colrts <- db[db[[MSDB.TAG.COL]] %in% col, MSDB.TAG.COLRT]
			rt[col] <- list(colrts)
		}

		if (.self$getRtUnit() == MSDB.RTUNIT.MIN)
			rt <- 60 * rt

		return(rt)
	})

} # end of load safe guard
