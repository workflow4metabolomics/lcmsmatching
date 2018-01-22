# vi: fdm=marker

if ( ! exists('MsBioDb')) { # Do not load again if already loaded

	library(methods)
	source('MsDb.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	MsBioDb <- setRefClass("MsBioDb", contains = "MsDb", fields = list(.biodb = "ANY", .massdb = "ANY"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	MsBioDb$methods( initialize = function(massdb = NULL, ...) {

		# Check bio database
		! is.null(massdb) || stop("You must set a bio database.")
		inherits(massdb, "biodb::MassdbConn") || stop("The bio database must inherit from MassdbConn class.")
		.biodb <<- biodb::Biodb$new()
		.massdb <<- .self$.biodb$getFactory()$createConn(massdb)

		callSuper(...)
	})

	###############################
	# GET CHROMATOGRAPHIC COLUMNS #
	###############################
	
	MsBioDb$methods( getChromCol = function(molid = NULL) {
		return(.self$.massdb$getChromCol(molid))
	})

}
