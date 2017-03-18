if ( ! exists('Ms4TabSqlDb')) { # Do not load again if already loaded

	library('methods')
	source('msdb-common.R')
	source('MsDb.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	Ms4TabSqlDb <- setRefClass("Ms4TabSqlDb", contains = "MsDb", fields = list(.host = "character", .port = "integer", .dbname = "character", .user = "character", .password = "character", .drv = "ANY", .conn = "ANY"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	Ms4TabSqlDb$methods( initialize = function(host = NA_character_, port = NA_integer_, dbname = NA_character_, user = NA_character_, password = NA_character_, ...) {

		# Initialize members
		.host <<- if ( ! is.null(host)) host else NA_character_
		.port <<- if ( ! is.null(port)) port else NA_integer_
		.dbname <<- if ( ! is.null(dbname)) dbname else NA_character_
		.user <<- if ( ! is.null(user)) user else NA_character_
		.password <<- if ( ! is.null(password)) password else NA_character_
		.drv <<- NULL
		.conn <<- NULL

		callSuper(...)
	})
	
	##################
	# GET CONNECTION #
	##################
	
	Ms4TabSqlDb$methods( .get.connection = function() {

		# Initialize connection
		if (is.null(.self$.conn)) {
			library('RPostgreSQL')
			.drv <<- dbDriver("PostgreSQL")
			.conn <<- dbConnect(.self$.drv, host = .self$.host, port = .self$.port, dbname = .self$.dbname, user = .self$.user, password = .self$.password)
		}

		return(.self$.conn)
	})
	
	##############
	# SEND QUERY #
	##############

	Ms4TabSqlDb$methods( .send.query = function(query) {
		conn <- .self$.get.connection() # Call it first separately, so library RPostgreSQL is loaded.
		rs <- try(dbSendQuery(conn, query))
		return(rs)
	})

	####################
	# GET MOLECULE IDS #
	####################
	
	Ms4TabSqlDb$methods( getMoleculeIds = function() {

		rs <- .self$.send.query('select pkmol.molecule_id as id from peaklist_name as pkmol;')
		ids <- fetch(rs,n=-1)
		ids <- ids[['id']] # Get 'id' column
		ids <- vapply(ids, function(x) { if (substring(x, 1, 1) == 'N') as.integer(substring(x, 2)) else as.integer(x) } , FUN.VALUE = 1, USE.NAMES = FALSE)
		ids <- (sort(ids))

		return(ids)
	})

	####################
	# GET NB MOLECULES #
	####################
	
	Ms4TabSqlDb$methods( getNbMolecules = function() {

		rs <- .self$.send.query('select count(*) from peaklist_name;')
		df <- fetch(rs,n=-1)
		n <- df[[1]]

		return(n)
	})
	
	#####################
	# GET MOLECULE NAME #
	#####################
	
	Ms4TabSqlDb$methods( getMoleculeName = function(molid) {

		# Build request
		where <- paste0(vapply(molid, function(id) paste0("pkmol.molecule_id = 'N", id, "'"), FUN.VALUE = ''), collapse = ' or ')
		request <- paste0('select pkmol.molecule_id as id, pkmol.name from peaklist_name as pkmol where ', where, ';')

		# Run request
		rs <- .self$.send.query(request)
		df <- fetch(rs,n=-1)

		# Get IDs
		ids <- vapply(df[['id']], function(x) as.integer(substring(x, 2)), FUN.VALUE = 1, USE.NAMES = FALSE)

		# Get names in the same order as the input vector
		names <- df[['name']][order(ids)[order(molid)]]

		return(if (is.null(names)) NA_character_ else names)
	})

	
	###############################
	# GET CHROMATOGRAPHIC COLUMNS #
	###############################
	
	Ms4TabSqlDb$methods( getChromCol = function(molid = NULL) {

		# Get all columns
		if (is.null(molid)) {
			request <- 'select name from method;'

		# Get columns of the specified molecules
		} else {
			where_molids <- paste0(vapply(molid, function(id) paste0("pkmol.molecule_id = 'N", id, "'"), FUN.VALUE = ''), collapse = ' or ')
			where <- paste0('pk.name_id = pkmol.id and pk.id = pkret.id_peak and pkret.id_method = method.id and (', where_molids, ')')
			request <- paste0('select distinct method.name from method, peaklist as pk, peaklist_name as pkmol, peaklist_ret as pkret where ', where, ';')
		}

		# Run request
		rs <- .self$.send.query(request)
		df <- fetch(rs,n=-1)

		# Gets column list
		cols <- df[['name']]

		# Remove FIA
		cols <- cols[ cols != 'FIA']

		# Normalize names
		cols <- vapply(cols, .normalize_column_name, FUN.VALUE = '', USE.NAMES = FALSE)

		# Remove duplicates
		cols <- cols[ ! duplicated(cols)]

		# Make data frame
		cols <- data.frame(id = cols, title = cols, stringsAsFactors = FALSE)

		return(cols)
	})

	################
	# FIND BY NAME #
	################

	Ms4TabSqlDb$methods( findByName = function(name) {

		if (is.null(name)) return(NA_integer_)

		# Put names in uppercase
		uname <- toupper(name)

		# Build request
		where <- paste0(vapply(uname, function(n) paste0("upper(pkmol.name) = '", gsub("'", "''", n, perl = TRUE), "'"), FUN.VALUE = '', USE.NAMES = FALSE), collapse = ' or ')
		request <- paste0('select pkmol.molecule_id as id, pkmol.name from peaklist_name as pkmol where ', where, ';')

		# Run request
		rs <- .self$.send.query(request)
		df <- fetch(rs,n=-1)

		# Adds missing names/IDs
		missing_names <- uname[ ! uname %in% toupper(df[['name']])]
		df <- rbind(df, data.frame(id = rep(NA_integer_, length(missing_names)), name = missing_names))

		# Get IDs and names
		ids <- vapply(df[['id']], function(x) as.integer(substring(x, 2)), FUN.VALUE = 1, USE.NAMES = FALSE)
		names <- toupper(as.character(df[['name']]))

		# Get IDs in the same order as the input vector
		ids[order(uname)] <- ids[order(names)]

		return(if (is.null(ids)) NA_integer_ else ids)
	})
	
	#######################
	# GET RETENTION TIMES #
	#######################
	
	Ms4TabSqlDb$methods( getRetentionTimes = function(molid, col = NA_character_) {

		if (is.null(molid) || is.na(molid) || length(molid)  != 1)
			stop("The parameter molid must consist only in a single integer.")
			
		# Build request
		request <- paste0("select distinct method.name as col, (pkret.retention * 60) as ret from peaklist as pk, peaklist_name as pkmol, peaklist_ret as pkret, method where pkret.id_peak = pk.id and pkmol.id = pk.name_id and pkret.id_method = method.id and pkmol.molecule_id = 'N", molid, "'")
		if ( ! is.na(col)) {
			where_cols <- paste0(vapply(col, function(c) paste0("method.name = '", c, "'"), FUN.VALUE = ''), collapse = ' or ')
			request <- paste0(request, ' and (', where_cols, ')')
		}
		request <- paste0(request, ';')

		# Run request
		rs <- .self$.send.query(request)
		df <- fetch(rs,n=-1)

		# Remove FIA
		df <- df[df[['col']] != 'FIA', ]

		# Normalize names
		df[['col']] <- vapply(df[['col']], .normalize_column_name, FUN.VALUE = '', USE.NAMES = FALSE)
 
		# Build output list
		lst <- list()
		if (nrow(df) > 0)
			for (i in 1:nrow(df)) {
				c <- df[i, 'col']
				lst[[c]] <- c(lst[[c]], df[i, 'ret'])
			}

		return(lst)
	})
	
	################
	# GET NB PEAKS #
	################
	
	Ms4TabSqlDb$methods( getNbPeaks = function(molid = NA_integer_, type = NA_character_) {

		# Build request
		request <- paste0("select count(*) from peaklist as pk, peaklist_name as pkmol where pkmol.id = pk.name_id")
		if ( length(molid) > 1 || ! is.na(molid)) {
			where_molids <- paste0(vapply(molid, function(id) paste0("pkmol.molecule_id = 'N", id, "'"), FUN.VALUE = ''), collapse = ' or ')
			request <- paste0(request, ' and (', where_molids, ')')
		}
		if ( ! is.na(type)) {
			request <- paste0(request, ' and ', if (type == MSDB.TAG.POS) '' else 'not ', 'ion_pos')
		}
		request <- paste0(request, ';')

		# Run request
		rs <- .self$.send.query(request)
		df <- fetch(rs,n=-1)

		return(df[1,1])
	})
	
	###############################
	# GET CHROMATOGRAPHIC COLUMNS #
	###############################
	
	Ms4TabSqlDb$methods( .to.dbcols = function(col) {

		# Get all column names
		request <- 'select name from method;'
		rs <- .self$.send.query(request)
		df <- fetch(rs,n=-1)

		# Get database column names
		dbcols <- df[['name']]
		dbcols <- dbcols[ dbcols != 'FIA']

		# Get normalize names
		normcols <- vapply(dbcols, .normalize_column_name, FUN.VALUE = '', USE.NAMES = FALSE)

		return(dbcols[normcols == tolower(col)])
	})
	
	#################
	# GET MZ VALUES #
	#################
	
	# Returns a numeric vector of all masses stored inside the database.
	Ms4TabSqlDb$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {

		# Build request
		select <- paste0("select distinct pk.mass as ", MSDB.TAG.MZTHEO)
		from <- " from peaklist as pk"
		where <- ""
		if ( ! is.null(mode))
			where <- paste0(" where ", if (mode == MSDB.TAG.POS) '' else 'not ', 'pk.ion_pos')
		limit <- ""
		if ( ! is.na(NA_integer_))
			limit <- paste(" limit", max.results)

		# Assemble request
		request <- paste0(select, from, where, ';')

		# Run request
		rs <- .self$.send.query(request)
		df <- fetch(rs, n=-1)

		return(df[[MSDB.TAG.MZTHEO]])
	})

	##########
	# SEARCH #
	##########

	Ms4TabSqlDb$methods( .do.search.for.mz.rt.bounds = function(mode, mz.low, mz.high, rt.low = NULL, rt.high = NULL, col = NULL, attribs = NULL, molids = NULL) {

		# Build request
		select <- paste0("select pkmol.molecule_id as ", MSDB.TAG.MOLID, ", pkmol.name as ", MSDB.TAG.MOLNAMES,", pk.mass as ", MSDB.TAG.MZTHEO, ", pk.composition as ", MSDB.TAG.COMP,", pk.attribution as ", MSDB.TAG.ATTR)
		from <- " from peaklist as pk, peaklist_name as pkmol"
		where <- paste0(" where pkmol.id = pk.name_id and pk.mass >= ", mz.low, " and pk.mass <= ", mz.high)
		where <- paste0(where, ' and ', if (mode == MSDB.TAG.POS) '' else 'not ', 'pk.ion_pos')

		# Insert where clause on attribs
		if ( ! is.null(attribs)) {
			where.attribs <- paste0(vapply(attribs, function(a) paste0("pk.attribution = '", a, "'"), FUN.VALUE = '', USE.NAMES = FALSE), collapse = " or ")
			where <- paste0(where, ' and (', where.attribs, ')')
		}

		# Insert where clause on molids
		if ( ! is.null(molids)) {
			where.molids <- paste0(vapply(molids, function(id) paste0("pkmol.molecule_id = 'N", id, "'"), FUN.VALUE = ''), collapse = ' or ')
			where <- paste0(where, ' and (', where.molids, ')')
		}

		# Insert where clause on columns
		if ( ! is.null(col)) {
			dbcols <- .self$.to.dbcols(col)
			if ( ! is.null(dbcols)) {

				# Can't find specified columns
				if (length(dbcols) == 0 && length(col) > 0)
					return(.get.empty.result.df(rt = TRUE))

				select <- paste0(select, ", (60 * pkret.retention) as ", MSDB.TAG.COLRT, ", method.name as ", MSDB.TAG.COL)
				from <- paste0(from, ", method, peaklist_ret as pkret")
				where.cols <- if (length(dbcols) == 0) 'TRUE' else paste0(vapply(dbcols, function(c) paste0("method.name = '", c, "'"), FUN.VALUE = '', USE.NAMES = FALSE), collapse = " or ")
				where <- paste0(where, " and pk.id = pkret.id_peak and pkret.id_method = method.id and (", where.cols, ")")
				if (! is.null(rt.low) && ! is.null(rt.high))
					where <- paste0(where, " and pkret.retention * 60 >= ", rt.low, " and pkret.retention * 60 <= ", rt.high)
			}
		}

		# Assemble request
		request <- paste0(select, from, where, ';')

		# Run request
		rs <- .self$.send.query(request)
		df <- fetch(rs,n=-1)

		# No results

		# Remove N prefix from IDs
		if (nrow(df) > 0)
			df[[MSDB.TAG.MOLID]] <- vapply(df[[MSDB.TAG.MOLID]], function(x) substring(x, 2), FUN.VALUE = '', USE.NAMES = FALSE)
		else if (nrow(df) == 0)
			df <- .get.empty.result.df(rt = ! is.null(col))

		return(df)
	})
	
} # end of load safe guard
