if ( ! exists('extract.address')) {

	###################
	# EXTRACT ADDRESS #
	###################

	extract.address <- function(url) {

		addr <- sub('^([0-9A-Za-z.]+).*$', '\\1', url, perl = TRUE)

		return(addr)
	}

	################
	# EXTRACT PORT #
	################

	extract.port <- function(url) {

		port <- sub('^.*:([0-9]+)$', '\\1', url, perl = TRUE)

		return(as.integer(port))
	}
}
