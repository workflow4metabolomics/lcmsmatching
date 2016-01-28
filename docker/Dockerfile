FROM ubuntu:14.04
#FROM ansible/ubuntu14.04-ansible:stable

MAINTAINER Pierrick Roger (pierrick.roger@gmail.com)

# Install Ansible
RUN apt-get update
RUN apt-get install -y ansible
RUN echo '[local]\nlocalhost\n' > /etc/ansible/hosts
RUN apt-get install -y python-apt
#RUN apt-get update -f
#RUN apt-get install -y aptitude

ADD ansible /files/ansible

# Run ansible role
WORKDIR /files/ansible
RUN ansible-playbook lcmsmatching.yml -c local

#RUN apt-get install -y r-base

## Install R
#RUN echo "deb http://mirrors.ebi.ac.uk/CRAN/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list && \
#	gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9 && \
#	gpg -a --export E084DAB9 | sudo apt-key add -
#
## R and Library Dependencies
#RUN apt-get update && apt-get -y upgrade && apt-get install -y \
#	r-base r-base-dev
#RUN apt-get install -y libcurl4-openssl-dev

## Add automatic repo finder for R:
## TODO Replace the following call by a call to an installation script provided inside the sources
#RUN R -e "install.packages(c('stringr', 'xlsx', 'getopt', 'plyr'), dependencies = TRUE, repos='http://lib.ugent.be/CRAN/')"
#RUN R -e "install.packages('XML',   dependencies = TRUE, repos='http://lib.ugent.be/CRAN/')"

# Install
#RUN chmod +x /scripts/installTool
#RUN /scripts/installTool

# Add script sources
ADD w4m-tool-lcmsmatching-2.0.tar.gz /files/lcmsmatching/

# Clean up
RUN apt-get clean && apt-get autoremove -y && rm -rf /var/lib/{apt,dpkg,cache,log}/ /tmp/* /var/tmp/*

# Define Entry point script
RUN chmod +x /files/lcmsmatching/search-mz
ENTRYPOINT ["/files/lcmsmatching/search-mz"]
