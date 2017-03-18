if ( ! exists('MsDbOutputStream')) { # Do not load again if already loaded

	library('methods')
	source('msdb-common.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	MsDbOutputStream <- setRefClass("MsDbOutputStream", fields = list(.keep.unused = "logical", .one.line = "logical", .match.sep = "character", .output.fields = "ANY", .multval.field.sep = "character", .first.val = "logical", .ascii = "logical", .noapostrophe = "logical", .noplusminus = "logical", .nogreek = "logical", .rtunit = 'character'))
	
	###############
	# CONSTRUCTOR #
	###############
	
	#' Constructor.
	#'
	#' @param keep.unused   Set to \code{TRUE} if you want to keep in the output, unused columns of the input.
	#' @param one.line      Set to \code{TRUE} if you want to output only one line for each input line.
	#' @return
	#' @examples
	#' stream <- MsDbOutputDataFrameStream$new(one.line = TRUE)
	MsDbOutputStream$methods( initialize = function(keep.unused = FALSE, one.line = FALSE, match.sep = MSDB.DFT.MATCH.SEP, output.fields = msdb.get.dft.output.fields(), multval.field.sep = MSDB.DFT.OUTPUT.MULTIVAL.FIELD.SEP, first.val = FALSE, ascii = FALSE, noapostrophe = FALSE, noplusminus = FALSE, nogreek = FALSE, rtunit = MSDB.RTUNIT.SEC, ...) {

		callSuper(...)
		
		.keep.unused <<- keep.unused
		.one.line <<- one.line
		.match.sep <<- match.sep
		.output.fields <<- output.fields
		.multval.field.sep <<- multval.field.sep
		.first.val <<- first.val
		.ascii <<- ascii
		.noapostrophe <<- noapostrophe
		.noplusminus <<- noplusminus
		.nogreek <<- nogreek
		.rtunit <<- rtunit
	})
	
	#################
	# MATCHED PEAKS #
	#################
	
	MsDbOutputStream$methods( matchedPeaks = function(mz, rt = NULL, unused = NULL, peaks = NULL) {
		stop("Method matchedPeaks() not implemented in concrete class.")
	})

} # end of load safe guard
