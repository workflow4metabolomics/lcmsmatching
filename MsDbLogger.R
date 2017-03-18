if ( ! exists('MsDbLogger')) { # Do not load again if already loaded

	source('MsDbObserver.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	MsDbLogger <- setRefClass("MsDbLogger", contains = 'MsDbObserver', fields = list(.verbose = 'numeric', .file = 'ANY' ))
	
	###############
	# CONSTRUCTOR #
	###############
	
	MsDbLogger$methods( initialize = function(verbose = 1, file = NULL, ...) {
	
		.verbose <<- if ( ! is.null(verbose) && ! is.na(verbose)) verbose else 1
		.file <<- if ( ! is.null(file) && ! is.na(file)) file else stderr()
	
		callSuper(...) # calls super-class initializer with remaining parameters
	})
	
	############
	# PROGRESS #
	############
	
	MsDbLogger$methods( progress = function(msg, level = 1) {
		if (.self$.verbose >= level)
			cat(msg, "\n", sep = '', file = .self$.file)
	})

} # end of load safe guard
