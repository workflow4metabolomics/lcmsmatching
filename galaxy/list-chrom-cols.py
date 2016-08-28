#!/usr/bin/env python

import argparse
import os

def get_chrom_cols(dbtype, dburl, dbtoken = None, dbfields = None, dbmsmodes = None):
    command = "../r-msdb/search-mz -d " + dbtype + " --url '" + dburl + "'"
    if dbtoken is not None:
        command += " --db-token " + dbtoken
    if dbfields is not None:
        command += " --db-fields '" + dbfields + "'"
    if dbmsmodes is not None:
        command += " --db-ms-modes '" + dbmsmodes + "'"
    command += " --list-cols"
    print(command)
    os.system(command)

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
    
    get_chrom_cols(**args_dict)
