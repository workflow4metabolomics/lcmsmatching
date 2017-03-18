if ( ! exists('HtmlWriter')) { # Do not load again if already loaded

	library(methods)

	#####################
	# CLASS DECLARATION #
	#####################
	
	HtmlWriter <- setRefClass("HtmlWriter", fields = list(.file = "character", .auto.indent = "numeric"))
	
	
	###############
	# CONSTRUCTOR #
	###############
	
	HtmlWriter$methods( initialize = function(file = NA_character_, auto.indent = TRUE, ...) {

		.file <<- file
		.auto.indent <<- if (auto.indent) 0 else NA_integer_

		# Create empty file
		cat('', file = .self$.file, append = FALSE)

		callSuper(...) # calls super-class initializer with remaining parameters
	})

	#########
	# WRITE #
	#########

	HtmlWriter$methods( write = function(text, indent = NA_integer_, newline = TRUE, escape = FALSE) {

		# Compute indentation
		if (is.na(indent))
			indent <- if (is.na(.self$.auto.indent)) 0 else .self$.auto.indent

		cat(rep("\t", indent), text, if (newline) "\n" else "", sep = '', file = .self$.file, append = TRUE)
	})

	#############
	# WRITE TAG #
	#############

	HtmlWriter$methods( writeTag = function(tag, attr = NA_character_, text = NA_character_, indent = NA_integer_, newline = TRUE) {

		if (is.na(text)) {
			attributes <- if (is.na(attr)) '' else paste0(' ', paste(vapply(names(attr), function(a) paste0(a, '="', attr[[a]], '"'), FUN.VALUE=''), collapse = ' '))
			.self$write(paste0("<", tag, attributes, "/>"), indent = indent, newline = newline, escape = FALSE)
		}
		else {
			.self$writeBegTag(tag, attr = attr, indent = indent, newline = FALSE)
			.self$write(text, escape = TRUE , indent = 0, newline = FALSE)
			.self$writeEndTag(tag, indent = 0, newline = newline)
		}
	})

	###################
	# WRITE BEGIN TAG #
	###################

	HtmlWriter$methods( writeBegTag = function(tag, attr = NA_character_, indent = NA_integer_, newline = TRUE) {

		# Write opening tag
		attributes <- if (is.na(attr)) '' else paste0(' ', paste(vapply(names(attr), function(a) paste0(a, '="', attr[[a]], '"'), FUN.VALUE=''), collapse = ' '))
		.self$write(paste0("<", tag, attributes, ">"), indent = indent, newline = newline, escape = FALSE)

		# Increment auto-indent
		if ( ! is.na(.self$.auto.indent))
			.auto.indent <<- .self$.auto.indent + 1
	})

	#################
	# WRITE END TAG #
	#################

	HtmlWriter$methods( writeEndTag = function(tag, indent = NA_integer_, newline = TRUE) {

		# Decrement auto-indent
		if ( ! is.na(.self$.auto.indent))
			.auto.indent <<- .self$.auto.indent - 1

		# Write closing tag
		.self$write(paste0("</", tag, ">"), indent = indent, newline = newline, escape = FALSE)
	})

	###############
	# WRITE TABLE #
	###############

	HtmlWriter$methods( writeTable = function(x, indent = NA_integer_, newline = TRUE) {

		.self$writeBegTag('table', indent = indent, newline = newline)

		# Write table header
		if ( ! is.null(colnames(x))) {
			.self$writeBegTag('tr', indent = indent + 1, newline = newline)
			for (field in colnames(x))
				.self$writeTag('th', text = field, indent = indent + 2, newline = newline)
			.self$writeEndTag('tr', indent = indent + 1, newline = newline)
		}

		# Write values
		if (nrow(x) > 0 && ncol(x) > 0)
			for (i in 1:nrow(x)) {
				.self$writeBegTag('tr', indent = indent + 1, newline = newline)
				for (j in 1:ncol(x))
					.self$writeTag('td', text = x[i, j], indent = indent + 2, newline = newline)
				.self$writeEndTag('tr', indent = indent + 1, newline = newline)
			}
		.self$writeEndTag('table', indent = indent, newline = newline)
	})


} # end of load safe guard
