#!/usr/bin/env python

import csv
import re

def get_file_cols(file, preferred):

	cols = []

	with open(file if isinstance(file, str) else file.get_file_name(), 'rb') as f:

		# Read file header
		reader = csv.reader(f, delimiter = "\t", quotechar='"')
		header = reader.next()
		header.insert(0, 'NA')

		# Determine default value
		default = 0
		partial_match = -1
		for p in preferred:
			for i, c in enumerate(header):
				if c == p:
					default = i # Perfect match !
					break
				if partial_match < 0 and re.match(p, c):
					partial_match = i # Keep this partial match in case we find no perfect match
			if default != 0:
				break
		if default == 0 and partial_match > 0:
			default = partial_match

		# Build list of cols
		for i, c in enumerate(header):
                    cols.append( (c, c, i == default) )

	return cols
