LC/MS matching
==============

[![Build Status](https://travis-ci.org/workflow4metabolomics/lcmsmatching.svg?branch=master)](https://travis-ci.org/workflow4metabolomics/lcmsmatching)

An LC/MS matching tool for [Galaxy](https://galaxyproject.org/), part of the [Workflow4Metabolomics](http://workflow4metabolomics.org/) project, and developed during the [MetaboHUB](http://www.metabohub.fr/en) project.

The two matching algorithms used in this tool have been imported from developments made at [CEA](http://www.cea.fr/english) Saclay, inside the *DSV/IBITEC-S/SPI*. They have been translated from C# to R.

For more information, see the galaxy tool page, help section, available inside `galaxy/lcmsmatching.xml`.

## search-mz

This is the script, included in this repository, that allows run on command line an MZ matching on one of the available database types.

Please run `search-mz -h` for a help page listing all options and presenting some examples.

## Dependencies

 * `libssl-dev`.
 * `libcurl4-openssl-dev`.
 * `libxml2-dev`.
 * `R` version `3.2.2`.
 * `R` packages:
   - `getopt` >= `1.20.0`.
   - `stringr` >= `1.0.0`.
   - `plyr` >= `1.8.3`.
   - `XML` >= `3.98`.
   - `bitops` >= `1.0_6`.
   - `RCurl` >= `1.95`.
   - `jsonlite` >= `1.1`.

## Updates

### 3.3.0

   * The file database (in-house) field names are now presented in individual choice lists instead of a single text box where you had to insert a very long keys/values string.
   * The tool now tries to guess the names of the file database fields, the values of the MS mode column, and the names of the input file columns.
   * Allows to select the unit (minutes or seconds) of retention time values inside the input file, but also inside the file database (in-house).
