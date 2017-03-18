if ( ! exists('extname')) { # Do not load again if already loaded

	source('strhlp.R')

	###########
	# EXTNAME #
	###########

	extname <- function(path) {
		return(sub('^.*\\.([^.]*)$', '\\1', path, perl = TRUE))
	}

	##############
	# REMOVE EXT #
	##############

	remove.ext <- function(path) {
		return(sub('\\.[^.]*$', '', path))
	}
}
