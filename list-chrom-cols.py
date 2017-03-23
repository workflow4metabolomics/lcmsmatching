#!/usr/bin/env python
# vi: fdm=marker

import argparse
import subprocess
import re
import urllib2
import json
import csv

# Get chrom cols {{{1
################################################################

def get_chrom_cols(dbtype, dburl, dbtoken = None, col_field = 'chromcol'):
    
    cols = []
    
    if dbtype == 'peakforest':
        url = dburl + ( '' if dburl[-1] == '/' else '/' ) + 'metadata/lc/list-code-columns'
        if dbtoken is not None:
            url += '?token=' + dbtoken
        result = urllib2.urlopen(url).read()
        v = json.JSONDecoder().decode(result)
        for colid, coldesc in v.iteritems():
            s = coldesc['name'] + ' - ' + coldesc['constructor'] + ' - L' + str(coldesc['length']) + ' - diam. ' + str(coldesc['diameter']) + ' - part. ' + str(coldesc['particule_size']) + ' - flow ' + str(coldesc['flow_rate'])
            cols.append( (s , colid, False) )
        
    elif dbtype == 'inhouse':
                
        # Get all column names from file
        with open(dburl if isinstance(dburl, str) else dburl.get_file_name(), 'r') as dbfile:
            reader = csv.reader(dbfile, delimiter = "\t", quotechar='"')
            header = reader.next()
            if col_field in header:
                i = header.index(col_field)
                allcols = []
                for row in reader:
                    col = row[i]
                    if col not in allcols:
                        allcols.append(col)
                for i, c in enumerate(allcols):
                    cols.append( (c, c, False) )
    
    return cols

# Main {{{1
################################################################

if __name__ == '__main__':
    
    # Parse command line arguments
    parser = argparse.ArgumentParser(description='Script for getting chromatographic columns of an RMSDB database for Galaxy tool lcmsmatching.')
    parser.add_argument('-d', help = 'Database type',       dest = 'dbtype',    required = True)
    parser.add_argument('-u', help = 'Database URL',        dest = 'dburl',     required = True)
    parser.add_argument('-t', help = 'Database token',      dest = 'dbtoken',   required = False)
    parser.add_argument('-f', help = 'Chromatogrphic column field name',     dest = 'col_field',  required = False)
    args = parser.parse_args()
    args_dict = vars(args)
    
    print(get_chrom_cols(**args_dict))
