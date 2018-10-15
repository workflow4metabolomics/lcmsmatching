#!/bin/bash

version=$(grep r-biodb lcmsmatching.xml | sed 's/^.*version="\([^"]*\)".*$/\1/')
R -e "devtools::install_github('pkrog/biodb', ref = 'v$version')"
