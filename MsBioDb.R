if ( ! exists('MsBioDb')) { # Do not load again if already loaded

	library(methods)
	source('MsDb.R')
	source('BiodbObject.R', chdir = TRUE)
	source('BiodbFactory.R', chdir = TRUE)

	#####################
	# CLASS DECLARATION #
	#####################
	
	MsBioDb <- setRefClass("MsBioDb", contains = "MsDb", fields = list(.massdb = "ANY"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	MsBioDb$methods( initialize = function(massdb = NULL, ...) {

		# Check bio database
		! is.null(massdb) || stop("You must set a bio database.")
		inherits(massdb, "MassdbConn") || stop("The bio database must inherit from MassdbConn class.")
		.massdb <<- massdb

		callSuper(...)
	})

	####################
	# HANDLE COMPOUNDS #
	####################
	
	MsBioDb$methods( handleCompounds = function() {
		return(.self$.massdb$handlesEntryType(BIODB.COMPOUND))
	})

	####################
	# GET MOLECULE IDS #
	####################
	
	MsBioDb$methods( getMoleculeIds = function(max.results = NA_integer_) {
		return(.self$.massdb$getEntryIds(type = BIODB.COMPOUND, max.results = max.results))
	})

	####################
	# GET NB MOLECULES #
	####################
	
	MsBioDb$methods( getNbMolecules = function() {
		return(.self$.massdb$getNbEntries(type = BIODB.COMPOUND))
	})

	#################
	# GET MZ VALUES #
	#################

	MsBioDb$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {
		return(.self$.massdb$getMzValues(mode = mode, max.results = max.results))
	})

	#####################
	# GET MOLECULE NAME #
	#####################

	MsBioDb$methods( getMoleculeName = function(molid) {
		return(.self$.massdb$getMoleculeName(molid))
	})

	###############################
	# GET CHROMATOGRAPHIC COLUMNS #
	###############################
	
	MsBioDb$methods( getChromCol = function(molid = NULL) {
		return(.self$.massdb$getChromCol(molid))
	})

	################
	# FIND BY NAME #
	################

	MsBioDb$methods( findByName = function(name) {
		return(.self$.massdb$findCompoundByName(name))
	})
	
	#######################
	# GET RETENTION TIMES #
	#######################
	
	MsBioDb$methods( getRetentionTimes = function(molid, col = NA_character_) {
		return(.self$.massdb$getRetentionTimes(molid, chrom.cols = col))
	})
	
	################
	# GET NB PEAKS #
	################
	
	MsBioDb$methods( getNbPeaks = function(molid = NA_integer_, mode = NA_character_) {
		return(.self$.massdb$getNbPeaks(compound.ids = molid, mode = mode))
	})

}
