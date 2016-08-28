#!/usr/bin/env python

import argparse
import subprocess
import re

def get_chrom_cols(dbtype, dburl, dbtoken = None, dbfields = None, dbmsmodes = None):
    
    # Construct command to run
    command = ["../r-msdb/search-mz", "-d", dbtype, "--url", dburl]
    if dbtoken is not None:
        command.extend(["--db-token", dbtoken])
    if dbfields is not None:
        command.extend(["--db-fields", dbfields])
    if dbmsmodes is not None:
        command.extend(["--db-ms-modes", dbmsmodes])
    command.append("--list-cols")
    
    # Run command and get output
    cols_table = subprocess.check_output(' '.join(command), shell = True)
    
    # Parse each output lines and fill the list
    cols = []
    i = 0
    for i, line in enumerate(cols_table.split("\n")[1:]):
        m = re.search('^"(.*)"\s*"(.*)"$', line)
        if m is not None:
            value = m.group(1)
            title = m.group(2)
            cols.append( (title, value, i == 0) )
    
    return cols

########
# MAIN #
########

if __name__ == '__main__':
    
    # Parse command line arguments
    parser = argparse.ArgumentParser(description='Script for getting chromatographic columns of an RMSDB database for Galaxy tool lcmsmatching.')
    parser.add_argument('-d', help = 'Database type',       dest = 'dbtype',    required = True)
    parser.add_argument('-u', help = 'Database URL',        dest = 'dburl',     required = True)
    parser.add_argument('-t', help = 'Database token',      dest = 'dbtoken',   required = False)
    parser.add_argument('-f', help = 'Database fields',     dest = 'dbfields',  required = False)
    parser.add_argument('-m', help = 'Database MS modes',   dest = 'dbmsmodes', required = False)
    args = parser.parse_args()
    args_dict = vars(args)
    
    print(get_chrom_cols(**args_dict))
