# vi: fdm=marker

source(file.path(dirname(script.path), '..', 'MsDbInputDataFrameStream.R'), chdir = TRUE)
source(file.path(dirname(script.path), '..', 'MsDbOutputDataFrameStream.R'), chdir = TRUE)
source(file.path(dirname(script.path), '..', 'msdb-common.R'), chdir = TRUE)

test.10.all.mol.ids <- function() {
	if (get.db()$handleCompounds()) {
		molids <- get.db()$getMoleculeIds(max.results = 10)
		checkTrue(length(molids) > 0)
		checkTrue( all( ! duplicated(molids)))
	}
}

test.15.nb.mols <- function() {
	if (get.db()$handleCompounds()) {
		checkTrue(get.db()$getNbMolecules() > 0)
		checkTrue(get.db()$getNbMolecules() == length(get.db()$getMoleculeIds()))
	}
}

test.mol.names <- function() {
	molids <- get.db()$getMoleculeIds()
	checkTrue(is.na(get.db()$getMoleculeName(NA_character_)))
	checkTrue(is.na(get.db()$findByName(NA_character_)[[1]]))
	submolids <- molids[1:10] # get subset of molids
	checkTrue(length(get.db()$getMoleculeName(submolids)) == length(submolids))
	submolids <- c(molids[1:10], NA_character_) # get subset of molids
	checkTrue(length(get.db()$getMoleculeName(submolids)) == length(submolids))
	for (molid in molids[1:20]) {
		name <- get.db()$getMoleculeName(molid)
		checkTrue( ( ! is.na(molid) && ! is.na(name)) || (is.na(molid) && is.na(name)) )
		if ( ! is.na(molid))
			checkTrue(molid %in% get.db()$findByName(name)[[1]])
	}
}

test.unknown.mol <- function() {
	molids <- get.db()$getMoleculeIds()
	for (molid in 1:20)
		if ( ! molid %in% molids)
			checkTrue(is.na(get.db()$getMoleculeName(molid)))
}

test.findbyname <- function() {
	checkTrue(is.na(get.db()$findByName(NULL)))

	molids <- get.db()$getMoleculeIds()
	molids <- molids[1:10]
	names <- get.db()$getMoleculeName(molids)
	ids.of.names <- get.db()$findByName(names)
	checkTrue(length(molids) == length(ids.of.names))
	for (i in seq(molids))
		checkTrue(molids[[i]] %in% ids.of.names[[i]])
}

test.columns <- function() {
	cols <- get.db()$getChromCol()
	checkTrue(class(cols) == 'data.frame')
	checkTrue(nrow(cols) > 0)
	checkTrue('id' %in% colnames(cols))
	checkTrue('title' %in% colnames(cols))
}

test.columns.molid <- function() {
	if (get.db()$handleCompounds()) {
		molids <- get.db()$getMoleculeIds()
		cols <- get.db()$getChromCol(molids[1:100])
		checkTrue(nrow(cols) >= 0)
	}
}

test.peaks <- function() {
	checkTrue(get.db()$getNbPeaks() > 0)
	checkTrue(get.db()$getNbPeaks(type = MSDB.TAG.POS) > 0)
	checkTrue(get.db()$getNbPeaks(type = MSDB.TAG.NEG) > 0)
	molids <- get.db()$getMoleculeIds()
	submolids <- molids[1:100] # get subset of molids
	checkTrue(get.db()$getNbPeaks(molid = submolids) >= 0)
	checkTrue(get.db()$getNbPeaks(molid = submolids, type = MSDB.TAG.POS) >= 0)
	checkTrue(get.db()$getNbPeaks(molid = submolids, type = MSDB.TAG.NEG) >= 0)
}

long.test.peaks <- function() {
	molids <- get.db()$getMoleculeIds()
	for (i in molids) {
		if (get.db()$getNbPeaks(molid = i) > 0) {
			checkTrue(get.db()$getNbPeaks(molid = i, type = MSDB.TAG.POS) > 0 || get.db()$getNbPeaks(molid = i, type = MSDB.TAG.NEG) > 0)
			break;
		}
	}
}

test.rt <- function() {
	checkTrue( ! is.null(get.db()$getChromCol()))
	checkTrue(nrow(get.db()$getChromCol()) > 0)

	# Get all molecule ids
	molids <- get.db()$getMoleculeIds()
	badids <- (1:10000)[! 1:10000 %in% molids]

	for (badid in badids[1:10]) {
		if ( ! is.na(badid)) {
			checkTrue(length(get.db()$getRetentionTimes(badid)) == 0)
			checkTrue(nrow(get.db()$getChromCol(badid)) == 0)
		}
	}

	for (molid in molids[1:10]) {
		if ( ! is.na(molid)) {
			checkTrue( ! is.null(get.db()$getRetentionTimes(molid)))
			checkTrue( ! is.null(get.db()$getChromCol(molid)))
		}
	}
}

test.search.mz.no.result <- function() {
	mzvals <- get.db()$getMzValues(mode = MSDB.TAG.POS, max.results = 100)
	checkTrue(length(mzvals) <= 100)
	r <- get.db()$searchForMzRtList(x = msdb.make.input.df(mz = mzvals[1] - 10), mode = MSDB.TAG.POS, prec = 5, shift = 0)
	checkTrue(MSDB.TAG.MOLID %in% colnames(r))
	checkTrue(MSDB.TAG.MOLNAMES %in% colnames(r))
	checkTrue(MSDB.TAG.MZ %in% colnames(r))
	checkTrue(MSDB.TAG.MZTHEO %in% colnames(r))
	checkTrue(MSDB.TAG.ATTR %in% colnames(r))
	checkTrue(MSDB.TAG.COMP %in% colnames(r))
	checkTrue(nrow(r) == 1)
	checkTrue(is.na(r[1, MSDB.TAG.MOLID]))
}

test.search.mzrt.no.result <- function() {
	mzvals <- get.db()$getMzValues(mode = MSDB.TAG.POS, max.results = 100)
	checkTrue(length(mzvals) <= 100)
	r <- get.db()$searchForMzRtList(x = msdb.make.input.df(mz = mzvals[1] - 10, rt = 8.9), mode = MSDB.TAG.POS, prec = 5, shift = 0, rt.tol.x = 5, rt.tol.y = 0.8, col = c('blabla', 'yep'))
	checkTrue(MSDB.TAG.MOLID %in% colnames(r))
	checkTrue(MSDB.TAG.MOLNAMES %in% colnames(r))
	checkTrue(MSDB.TAG.MZ %in% colnames(r))
	checkTrue(MSDB.TAG.RT %in% colnames(r))
	checkTrue(MSDB.TAG.MZTHEO %in% colnames(r))
	checkTrue(MSDB.TAG.ATTR %in% colnames(r))
	checkTrue(MSDB.TAG.COMP %in% colnames(r))
	checkTrue(MSDB.TAG.COLRT %in% colnames(r))
	checkTrue(MSDB.TAG.COL %in% colnames(r))
	checkTrue(nrow(r) == 1)
	checkTrue(is.na(r[1, MSDB.TAG.MOLID]))
}

test.search.mz <- function() {

	mzvals <- get.db()$getMzValues(mode = MSDB.TAG.POS, max.results = 100)
	checkTrue(length(mzvals) <= 100)
	mzvals <- sort(mzvals)

	for (mz in mzvals) {
		if ( ! is.na(mz)) {

			# Search
			r <- get.db()$searchForMzRtList(x = msdb.make.input.df(mz = mz), mode = MSDB.TAG.POS, prec = 5, shift = 0)
			checkTrue(nrow(r) >= 1)
			checkTrue( ! is.na(r[1, MSDB.TAG.MOLID]))

			checkTrue(nrow(get.db()$searchForMzRtList(msdb.make.input.df(mz = mz), mode = MSDB.TAG.POS, prec = 5, shift = 0)) >= 1)
			checkTrue(nrow(get.db()$searchForMzRtList(msdb.make.input.df(mz = mz), mode = MSDB.TAG.POS, prec = 100, shift = 0)) >= 1)
			checkTrue(nrow(get.db()$searchForMzRtList(msdb.make.input.df(mz = c(mz)), mode = MSDB.TAG.POS, prec = 100, shift = 0)) >= 1)

			break
		}
	}
}

test.search.mz.plain <- function() {

	mzvals <- get.db()$getMzValues(mode = MSDB.TAG.POS, max.results = 100)
	checkTrue(length(mzvals) <= 100)

	get.db()$setMzTolUnit(MSDB.MZTOLUNIT.PLAIN)

	for (mz in mzvals) {
		if ( ! is.na(mz)) {

			# Search
			r <- get.db()$searchForMzRtList(x = msdb.make.input.df(mz = mz), mode = MSDB.TAG.POS, prec = 0.1, shift = 0)
			checkTrue(nrow(r) >= 1)
			checkTrue( ! is.na(r[1, MSDB.TAG.MOLID]))

			checkTrue(nrow(get.db()$searchForMzRtList(msdb.make.input.df(mz = mz), mode = MSDB.TAG.POS, prec = 0.1, shift = 0)) >= 1)
			checkTrue(nrow(get.db()$searchForMzRtList(msdb.make.input.df(mz = mz), mode = MSDB.TAG.POS, prec = 1, shift = 0)) >= 1)
			checkTrue(nrow(get.db()$searchForMzRtList(msdb.make.input.df(mz = c(mz)), mode = MSDB.TAG.POS, prec = 1, shift = 0)) >= 1)

			break
		}
	}

	get.db()$setMzTolUnit(MSDB.MZTOLUNIT.PPM)
}

test.search.mzrt <- function() {

	mzvals <- get.db()$getMzValues(mode = MSDB.TAG.POS, max.results = 100)
	checkTrue(length(mzvals) <= 100)

	found_test <- FALSE
	for (mz in mzvals) {
		if ( ! is.na(mz)) {

			# Search with mz only
			r <- get.db()$searchForMzRtList(x = msdb.make.input.df(mz = mz), mode = MSDB.TAG.POS, prec = 5, shift = 0)
			checkTrue(nrow(r) >= 1)
			checkTrue(MSDB.TAG.MOLID %in% colnames(r))
			checkTrue(class(r[[MSDB.TAG.MOLID]]) == 'character')
			checkTrue(any( ! is.na(r[[MSDB.TAG.MOLID]])))

			# Loop on all molids obtained
			for (molid in r[[MSDB.TAG.MOLID]]) {

				if ( ! is.na(molid)) {
					# Get retention times of molecule
					rts <- get.db()$getRetentionTimes(molid)

					# Loop on all columns
					for (col in names(rts))
						for (rt in rts[[col]]) {
							r <- get.db()$searchForMzRtList(x = msdb.make.input.df(mz = mz, rt = rt, rtunit = get.db()$getRtUnit()), mode = MSDB.TAG.POS, prec = 5, shift = 0, col = col, rt.tol.x = 5, rt.tol.y = 0.8)
							checkTrue(nrow(r) >= 1)
							checkTrue(MSDB.TAG.RT %in% colnames(r))
							checkTrue(MSDB.TAG.COL %in% colnames(r))
							checkTrue(MSDB.TAG.COLRT %in% colnames(r))
							checkTrue(any( ! is.na(r[[MSDB.TAG.COL]])))
							checkTrue(any( ! is.na(r[[MSDB.TAG.COLRT]])))

							found_test <- TRUE
							break
						}

					# 

					if (found_test)
						break
				}
			}
			if (found_test)
				break
		}
		if (found_test)
			break
	}
}

# Test precursor search mz {{{1
################################################################

test.precursor.search.mz <- function() {

	# Get some mzvals
	mzvals <- get.db()$getMzValues(mode = MSDB.TAG.POS, max.results = 10)
	checkTrue(length(mzvals) <= 10)

	# Loop on all mz values obtained
	for (mz in mzvals) {
		if ( ! is.na(mz)) {

			# Search with this mz
			r <- get.db()$searchForMzRtList(x = msdb.make.input.df(mz = mz), mode = MSDB.TAG.POS, prec = 5, shift = 0)
			checkTrue(nrow(r) >= 1)
			checkTrue(MSDB.TAG.ATTR %in% colnames(r))
			checkTrue(class(r[[MSDB.TAG.ATTR]]) == 'character')

			# Loop on all peak attributions
			for (attr in r[[MSDB.TAG.ATTR]]) {

				# Search with this mz and the attribution as restriction
				r2 <- get.db()$searchForMzRtList(x = msdb.make.input.df(mz = mz), mode = MSDB.TAG.POS, prec = 5, shift = 0, attribs = attr)
				checkTrue(nrow(r2) >= 1)
				checkTrue(MSDB.TAG.ATTR %in% colnames(r2))
				checkTrue(all(r2[[MSDB.TAG.ATTR]] == attr))

				# Search with this mz and the attribution as precursor
				new.prec <- list()
				new.prec[[MSDB.TAG.POS]] <- c(attr)
				new.prec[[MSDB.TAG.NEG]] <- character(0)
				get.db()$setPrecursors(new.prec)
				r3 <- get.db()$searchForMzRtList(x = msdb.make.input.df(mz = mz), mode = MSDB.TAG.POS, prec = 5, shift = 0, precursor.match = TRUE)
				checkTrue(nrow(r3) >= 1)
				checkTrue(MSDB.TAG.ATTR %in% colnames(r3))
			}
		}
	}
}
