FROM ubuntu:14.04

MAINTAINER Pierrick Roger (pierrick.roger@gmail.com)

# Setup package repos
RUN echo "deb http://mirrors.ebi.ac.uk/CRAN/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# Update and upgrade system
RUN apt-get update
RUN apt-get -y upgrade

# Install R and other needed packages
RUN apt-get -y install r-base libcurl4-openssl-dev libxml2-dev
RUN R -e "install.packages(c('getopt', 'stringr', 'plyr', 'XML', 'RJSONIO'), lib='/usr/lib/R/library', dependencies = TRUE, repos='http://mirrors.ebi.ac.uk/CRAN')"

# Install tool
ADD tool.tar.gz /files/lcmsmatching/

# Clean up
RUN apt-get clean && apt-get autoremove -y && rm -rf /var/lib/{apt,dpkg,cache,log}/ /tmp/* /var/tmp/*

# Define Entry point script
ENTRYPOINT ["/files/lcmsmatching/search-mz"]
