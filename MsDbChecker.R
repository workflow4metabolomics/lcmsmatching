if ( ! exists('MsDbChecker')) { # Do not load again if already loaded

	source('MsDbObserver.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	MsDbChecker <- setRefClass("MsDbChecker", contains = 'MsDbObserver', fields = list(.fail = 'logical'))
	
	###############
	# CONSTRUCTOR #
	###############
	
	# fail  If set to TRUE, will fail (i.e.: quit application with a status set to 1) on error.
	MsDbChecker$methods( initialize = function(fail = FALSE, ...) {
	
		.fail <<- if ( ! is.null(fail) && ! is.na(fail)) fail else FALSE
	
		callSuper(...) # calls super-class initializer with remaining parameters
	})
	
	###########
	# WARNING #
	###########
	
	MsDbChecker$methods( warning = function(msg) {
		write(paste('WARNING: ', msg), stderr())
	})
	
	#########
	# ERROR #
	#########
	
	MsDbChecker$methods( error = function(msg) {

		write(paste('ERROR:', msg), stderr())

		# Fail
		if (.self$.fail)
			quit(status = 1)
	})
	
} # end of load safe guard
