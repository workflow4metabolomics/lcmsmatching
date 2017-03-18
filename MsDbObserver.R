if ( ! exists('MsDbObserver')) { # Do not load again if already loaded

	library('methods')

	#####################
	# CLASS DECLARATION #
	#####################
	
	MsDbObserver <- setRefClass("MsDbObserver", fields = list())
	
	############
	# PROGRESS #
	############
	
	MsDbObserver$methods( progress = function(msg, level = 1) {
	})
	
	###########
	# WARNING #
	###########
	
	MsDbObserver$methods( warning = function(msg) {
	})
	
	#########
	# ERROR #
	#########
	
	MsDbObserver$methods( error = function(msg) {
	})

} # end of load safe guard
