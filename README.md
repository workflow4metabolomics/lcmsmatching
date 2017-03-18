LC/MS matching
==============

[![Build Status](https://travis-ci.org/workflow4metabolomics/lcmsmatching.svg?branch=master)](https://travis-ci.org/workflow4metabolomics/lcmsmatching)

An LC/MS matching tool for [Galaxy](https://galaxyproject.org/), part of the [Workflow4Metabolomics](http://workflow4metabolomics.org/) project, and developed during the [MetaboHUB](http://www.metabohub.fr/en) project.

The two matching algorithms used in this tool have been imported from developments made at [CEA](http://www.cea.fr/english) Saclay, inside the *DSV/IBITEC-S/SPI*. They have been translated from C# to R.

For more information, see the galaxy tool page, help section, available inside `galaxy/lcmsmatching.xml`.
R-MSDB
======

RMsDb is a Mass Spectrometry database framework.

It includes a script called `search-mz` to search for annotation inside a database.

## Dependencies

 * libssl-dev
 * libcurl4-openssl-dev
 * libxml2-dev
 * R version 3.2.2
 * R packages:
   - getopt >= 1.20.0.
   - stringr >= 1.0.0.
   - plyr >= 1.8.3.
   - XML >= 3.98.
   - bitops >= `1.0_6` (when running on Peakforest).
   - RCurl >= `1.95` (when running on Peakforest).
   - RJSONIO >= 1.3.
