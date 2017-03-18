if ( ! exists('MsPeakForestDb')) { # Do not load again if already loaded

	library(methods)
	source('MsDb.R')
	source('UrlRequestScheduler.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	MsPeakForestDb <- setRefClass("MsPeakForestDb", contains = "MsDb", fields = list(.url = "character", .url.scheduler = "ANY", .token = "character"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	MsPeakForestDb$methods( initialize = function(url = NA_character_, useragent = NA_character_, token = NA_character_, ...) {

		callSuper(...)

		# Check URL
		if (is.null(url) || is.na(url))
		    stop("No URL defined for new MsPeakForestDb instance.")

		if (substring(url, nchar(url) - 1, 1) == '/')
			url <- substring(url, nchar(url) - 1)
		.url <<- url
		.url.scheduler <<- UrlRequestScheduler$new(n = 3, useragent = useragent)
		.self$.url.scheduler$setVerbose(1L)
		.token <<- token
		.rt.unit <<- MSDB.RTUNIT.MIN
	})

	###########
	# GET URL #
	###########

	MsPeakForestDb$methods( .get.url = function(url, params = NULL, ret.type = 'json') {

		res <- NULL

		# Add url prefix
		if (substring(url, 1, 1) == '/')
			url <- substring(url, 2)
		url <- paste(.self$.url, url, sep = '/')

		# Add token
		if ( ! is.na(.self$.token))
			params <- c(params, token = .self$.token)

		# Get URL
		content <- .self$.url.scheduler$getUrl(url = url, params = params)

		if (ret.type == 'json') {

			res <- jsonlite::fromJSON(content, simplifyDataFrame = FALSE)

			if (is.null(res)) {
				param.str <- if (is.null(params)) '' else paste('?', vapply(names(params), function(p) paste(p, params[[p]], sep = '='), FUN.VALUE = ''), collapse = '&', sep = '')
				stop(paste0("Failed to run web service. URL was \"", url, param.str, "\"."))
			}
		} else {
			if (ret.type == 'integer') {
				if (grepl('^[0-9]+$', content, perl = TRUE))
					res <- as.integer(content)
				else {
					res <- jsonlite::fromJSON(content, simplifyDataFrame = FALSE)
				}
			}
		}

		return(res)
	})

	####################
	# GET MOLECULE IDS #
	####################
	
	MsPeakForestDb$methods( getMoleculeIds = function() {

		ids <- as.character(.self$.get.url(url = 'compounds/all/ids'))

		return(ids)
	})

	####################
	# GET NB MOLECULES #
	####################
	
	MsPeakForestDb$methods( getNbMolecules = function() {

		n <- .self$.get.url(url = 'compounds/all/count', ret.type = 'integer')

		return(n)
	})
	
	###############################
	# GET CHROMATOGRAPHIC COLUMNS #
	###############################
	
	MsPeakForestDb$methods( getChromCol = function(molid = NULL) {

		# Set URL
		params <- NULL
		if ( ! is.null(molid))
			params <- list(molids = paste(molid, collapse = ','))

		# Call webservice
		wscols <- .self$.get.url(url = 'metadata/lc/list-code-columns', params = params)

		# Build data frame
		cols <- data.frame(id = character(), title = character())
		for(id in names(wscols))
			cols <- rbind(cols, data.frame(id = id, title = wscols[[id]]$name, stringsAsFactors = FALSE))

		return(cols)
	})
	
	#######################
	# GET RETENTION TIMES #
	#######################
	
	MsPeakForestDb$methods( getRetentionTimes = function(molid, col = NA_character_) {

		if (is.null(molid) || is.na(molid) || length(molid)  != 1)
			stop("The parameter molid must consist only in a single value.")

		rt <- list()

		# Set URL
		params <- NULL
		if ( ! is.null(molid))
			params <- list(molids = paste(molid, collapse = ','))

		# Call webservice
		spectra <- .self$.get.url(url = 'spectra/lcms/search', params = params)
		if (class(spectra) == 'list' && length(spectra) > 0) {
			for (s in spectra)
				if (is.na(col) || s$liquidChromatography$columnCode %in% col) {
					ret.time <- (s$RTmin + s$RTmax) / 2
					ret.time <- ret.time * 60 # Retention time are in minutes in Peakforest, but we want them in seconds
					c <- s$liquidChromatography$columnCode
					if (c %in% names(rt)) {
						if ( ! ret.time %in% rt[[c]])
							rt[[c]] <- c(rt[[c]], ret.time)
					} else
						rt[[c]] <- ret.time
				}
		}

		return(rt)
	})
	
	#####################
	# GET MOLECULE NAME #
	#####################

	MsPeakForestDb$methods( getMoleculeName = function(molid) {

		library(RJSONIO)

		if (is.null(molid))
			return(NA_character_)

		# Initialize names
		names <- as.character(molid)

		# Get non NA values
		non.na.molid <- molid[ ! is.na(molid)]

		if (length(non.na.molid) > 0) {
			# Set URL
			params <- c(molids = paste(non.na.molid, collapse = ','))

			# Call webservice
			names[ ! is.na(molid)] <- .self$.get.url(url = 'compounds/all/names', params = params)
		}

		return(names)
	})

	################
	# FIND BY NAME #
	################

	MsPeakForestDb$methods( findByName = function(name) {

		if (is.null(name))
			return(NA_character_)

		ids <- list()

		for (n in name) {

			if (is.na(n))
				ids <- c(ids, NA_character_)

			else {
				compounds <- .self$.get.url(url = paste0('search/compounds/name/', curlEscape(n)))$compoundNames
				ids <- c(ids, list(vapply(compounds, function(c) as.character(c$compound$id), FUN.VALUE = '')))
			}
		}

		return(ids)
	})

	#################
	# GET NB PEAKS #
	#################
	
	MsPeakForestDb$methods( getNbPeaks = function(molid = NA_integer_, type = NA_character_) {

		# Build URL
		params <- NULL
		if ( ! is.na(type))
			params <- c(params, mode = if (type == MSDB.TAG.POS) 'pos' else 'neg')
		if ( ! is.null(molid) && (length(molid) > 1 || ! is.na(molid)))
			params <- c(params, molids = paste(molid, collapse = ','))

		# Run request
		n <- .self$.get.url(url = 'spectra/lcms/count-peaks', params = params, ret.type = 'integer')

		return(sum(n))
	})
	
	#################
	# GET MZ VALUES #
	#################
	
	MsPeakForestDb$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {

		# Query params
		params <- NULL
		if ( ! is.null(mode))
			params <- c(params, mode = if (mode == MSDB.TAG.POS) 'positive' else 'negative')

		# Get MZ valuels
		mz <- .self$.get.url(url = 'spectra/lcms/peaks/list-mz', params = params)

		# Apply cut-off
		if ( ! is.na(max.results))
			mz <- mz[1:max.results]

		return(mz)
	})

	##############################
	# DO SEARCH FOR MZ RT BOUNDS #
	##############################

	MsPeakForestDb$methods( .do.search.for.mz.rt.bounds = function(mode, mz.low, mz.high, rt.low = NULL, rt.high = NULL, col = NULL, attribs = NULL, molids = NULL) {

		# Build URL for mz search
		url <- paste0('spectra/lcms/peaks/get-range/', mz.low, '/', mz.high)

		# Get spectra
		spectra <- .self$.get.url(url = url)

		# Build result data frame
		results <- data.frame(MSDB.TAG.MOLID = character(), MSDB.TAG.MOLNAMES = character(), MSDB.TAG.MOLMASS = numeric(), MSDB.TAG.MZTHEO = numeric(), MSDB.TAG.COMP = character(), MSDB.TAG.ATTR = character(), MSDB.TAG.INCHI = character(), MSDB.TAG.INCHIKEY = character(), MSDB.TAG.CHEBI = character(), MSDB.TAG.HMDB = character(), MSDB.TAG.KEGG = character(), MSDB.TAG.PUBCHEM = character())
		for (x in spectra) {
			if ('source' %in% names(x) && is.list(x$source))
				mztheo <- if ('mz' %in% names(x) && ! is.null(x$mz)) as.numeric(x$mz) else NA_real_
				comp <- if ('composition' %in% names(x) && ! is.null(x$composition)) x$composition else NA_character_
				attr <- if ('attribution' %in% names(x) && ! is.null(x$attribution)) x$attribution else NA_character_
				if ('listOfCompounds' %in% names(x$source)) {
					molids <- vapply(x$source$listOfCompounds, function(c) if ('id' %in% names(c) && ! is.null(c$id)) as.character(c$id) else NA_character_, FUN.VALUE = '')
					molnames <- vapply(x$source$listOfCompounds, function(c) if ('names' %in% names(c) && ! is.null(c$names)) paste(c$names, collapse = MSDB.MULTIVAL.FIELD.SEP) else NA_character_, FUN.VALUE = '')
					mass <- vapply(x$source$listOfCompounds, function(c) if ( ! 'averageMass' %in% names(c) || is.null(c$averageMass)) NA_real_ else as.double(c$averageMass), FUN.VALUE = 0.0)
					inchi <- vapply(x$source$listOfCompounds, function(c) if ( ! 'inChI' %in% names(c) || is.null(c$inChI)) NA_character_ else as.character(c$inChI), FUN.VALUE = '')
					inchikey <- vapply(x$source$listOfCompounds, function(c) if ( ! 'inChIKey' %in% names(c) || is.null(c$inChIKey)) NA_character_ else as.character(c$inChIKey), FUN.VALUE = '')
					chebi <- vapply(x$source$listOfCompounds, function(c) if ('ChEBI'  %in% names(c) && ! is.null(c$ChEBI)) as.character(c$ChEBI) else NA_character_, FUN.VALUE = '')
					chebi[chebi == 'CHEBI:null'] <- NA_character_
					hmdb <- vapply(x$source$listOfCompounds, function(c) if ('HMDB' %in% names(c) && ! is.null(c$HMDB)) as.character(c$HMDB) else NA_character_, FUN.VALUE = '')
					hmdb[hmdb == 'HMDBnull'] <- NA_character_
					kegg <- vapply(x$source$listOfCompounds, function(c) if ( ! 'KEGG' %in% names(c) || is.null(c$KEGG)) NA_character_ else as.character(c$KEGG), FUN.VALUE = '')
					pubchem <- vapply(x$source$listOfCompounds, function(c) if ( ! 'PubChemCID' %in% names(c) || is.null(c$PubChemCID)) NA_character_ else as.character(c$PubChemCID), FUN.VALUE = '')
					if (length(molids) > 0 && length(molids) == length(molnames))
						results <- rbind(results, data.frame(MSDB.TAG.MOLID = molids, MSDB.TAG.MOLNAMES = molnames, MSDB.TAG.MOLMASS = mass, MSDB.TAG.MZTHEO = mztheo, MSDB.TAG.COMP = comp, MSDB.TAG.ATTR = attr, MSDB.TAG.INCHI = inchi, MSDB.TAG.INCHIKEY = inchikey, MSDB.TAG.CHEBI = chebi, MSDB.TAG.HMDB = hmdb, MSDB.TAG.KEGG = kegg, MSDB.TAG.PUBCHEM = pubchem, stringsAsFactors = FALSE))
				}
		}

		# RT search
		if ( ! is.null(rt.low) && ! is.null(rt.high)) {

			rt.res <- data.frame(MSDB.TAG.MOLID = character(), MSDB.TAG.COL = character(), MSDB.TAG.COLRT = numeric())

			if (nrow(results) > 0) {

				# Build URL for rt search
				url <- paste0('spectra/lcms/range-rt-min/', rt.low / 60, '/', rt.high / 60)
				params <- NULL
				if ( ! is.null(col))
					params <- c(columns = paste(col, collapse = ','))

				# Run query
				rtspectra <- .self$.get.url(url = url, params = params)

				# Get compound/molecule IDs
				for (x in rtspectra)
					if (all(c('listOfCompounds', 'liquidChromatography') %in% names(x))) {
						molids <- vapply(x$listOfCompounds, function(c) if ('id' %in% names(c) && ! is.null(c$id)) as.character(c$id) else NA_character_, FUN.VALUE = '')
						if (length(molids) > 0) {
							col <- if ('columnCode' %in% names(x$liquidChromatography) && ! is.null(x$liquidChromatography$columnCode)) as.character(x$liquidChromatography$columnCode) else NA_character_
							rtmin <- if ('RTmin' %in% names(x) && ! is.null(x$RTmin)) as.double(x$RTmin) else NA_real_
							rtmax <- if ('RTmax' %in% names(x) && ! is.null(x$RTmax)) as.double(x$RTmax) else NA_real_
							colrt <- (rtmin + rtmax) / 2
							rt.res <- rbind(rt.res, data.frame(MSDB.TAG.MOLID = molids,
				                                   	   	   	   MSDB.TAG.COL = col,
				                                   	   	   	   MSDB.TAG.COLRT = colrt * 60,
					                                   	   	   stringsAsFactors = FALSE))
						}
					}
			}	

			# Add retention times and column info
			results <- merge(results, rt.res)
		}
		
		# Rename columns with proper names
		colnames(results) <- vapply(colnames(results), function(s) eval(parse(text=s)), FUN.VALUE = '')

		return(results)
	})
}
