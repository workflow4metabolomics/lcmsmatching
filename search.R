if ( ! exists('binary.search')) { # Do not load again if already loaded

	# Run a binary search on a sorted array.
	# val       The value to search.
	# tab       The array of values, sorted in ascending order.
	# lower     If set to NA, then search for the first value found by the binary search. If set to TRUE, find the value with the lowest index in the array. If set to FALSE, find the value with the highest index in the array.
	# first     The index of the array from which to start (1 by default).
	# last      The index of the array where to stop searching (end of the array by default).
	# Returns the index of the found value, or NA.
	binary.search <- function(val, tab, lower = NA, first = 1L, last = length(tab)) 
	{ 
		# Check array & value
		if (is.null(tab))
			stop('Argument "tab" is NULL.')
		if (is.null(val))
			stop('Argument "val" is NULL.')
	
		# Wrong arguments
		if (is.na(val) || last < first || length(tab) == 0)
			return(NA_integer_)
	
		# Find value
		l <- first
		h <- last
		while (h >= l) { 
	
			# Take middle point
			m <- (h + l) %/% 2
			# Found value
			if (tab[m] == val) {
				if (is.na(lower))
					return(m)
				if (lower && m > first) {
					for (i in (m-1):first)
						if (tab[i] != val)
							return(i+1)
				}
				else if ( ! lower && m < last)
					for (i in (m+1):last)
						if (tab[i] != val)
							return(i-1)
				return(m)
			}
			
			# Decrease higher bound
			else if (tab[m] > val) h <- m - 1
	
			# Increase lower bound
			else l <- m + 1
		} 
	
		# Value not found
		if ( ! is.na(lower)) {
			# Look for lower or higher bound
			if (lower)
				return(if (h < first) NA_integer_ else h)
			else
				return(if (l > last) NA_integer_ else l)
		}
		
		return(NA_integer_)
	} 

} # end of load safe guard
