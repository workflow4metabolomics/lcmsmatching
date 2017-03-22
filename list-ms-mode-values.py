#!/usr/bin/env python
# vi: fdm=marker

import csv
import re
import argparse

# Get MS mode values {{{1
################################################################

def get_ms_mode_value(file, col, preferred):

	modes = []
	cols = []
	preferred = preferred.split(',')

	with open(file if isinstance(file, str) else file.get_file_name(), 'r') as f:

		# Read file header
		reader = csv.reader(f, delimiter = "\t", quotechar='"')
		header = reader.next()
		try:
			index = header.index(col)
			for row in reader:
				v = row[index]
				if v not in modes:
					modes.append(v)

			# Find default value
			default = 0
			for p in preferred:
				for i, m in enumerate(modes):
					if m == p:
						default = i
						break
				if default != 0:
					break

			# Build list of cols
			for i, c in enumerate(modes):
				cols.append( (c, c, i == default) )
		except:
			pass

	return cols

# Main {{{1
################################################################

if __name__ == '__main__':
    
    # Parse command line arguments
    parser = argparse.ArgumentParser(description='Script for getting column names in a csv file.')
    parser.add_argument('-f', help = 'CSV File (separator must be TAB)',       dest = 'file',    required = True)
    parser.add_argument('-c', help = 'MS mode column name.',        dest = 'col',     required = True)
    parser.add_argument('-p', help = 'List (comma separated values) of preferred column names for default one.',        dest = 'preferred',     required = True)
    args = parser.parse_args()
    args_dict = vars(args)
    
    print(get_ms_mode_value(**args_dict))
