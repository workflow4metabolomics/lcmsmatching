#!/usr/bin/env python

import argparse
import subprocess
import re
import urllib2
import json

def get_chrom_cols(dbtype, dburl, dbtoken = None, dbfields = None):
    
    cols = []
    
    if dbtype == 'peakforest':
        url = dburl + 'metadata/lc/list-code-columns'
        if dbtoken is not None:
            url += '?token=' + dbtoken
        result = urllib2.urlopen(url).read()
        v = json.JSONDecoder().decode(result)
        i = 0
        for colid, coldesc in v.iteritems():
            cols.append( (coldesc['name'], colid, i == 0) )
            ++i
        
    elif dbtype == 'inhouse':
       pass         
    
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
    args = parser.parse_args()
    args_dict = vars(args)
    
    print(get_chrom_cols(**args_dict))
