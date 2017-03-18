if ( ! exists('MsDbInputStream')) { # Do not load again if already loaded

	library('methods')
	source('msdb-common.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	MsDbInputStream <- setRefClass("MsDbInputStream", fields = list(.input.fields = "ANY"))
	
	###############
	# CONSTRUCTOR #
	###############

	MsDbInputStream$methods( initialize = function(input.fields = msdb.get.dft.input.fields(), ...) {
		
		.input.fields <<- input.fields

		callSuper(...)
	})

	##########
	# GET MZ #
	##########

	MsDbInputStream$methods( getMz = function() {
		stop("Method getMz() not implemented in concrete class.")
	})

	##########
	# GET RT #
	##########

	MsDbInputStream$methods( getRt = function() {
		stop("Method getRt() not implemented in concrete class.")
	})

	###########
	# GET ALL #
	###########

	MsDbInputStream$methods( getAll = function(but = NULL) {
		stop("Method getUnused() not implemented in concrete class.")
	})

	###############
	# NEXT VALUES #
	###############

	MsDbInputStream$methods( nextValues = function() {
		stop("Method nextValues() not implemented in concrete class.")
	})

	###################
	# HAS NEXT VALUES #
	###################

	MsDbInputStream$methods( hasNextValues = function() {
		stop("Method hasNextValues() not implemented in concrete class.")
	})

} # end of load safe guard
