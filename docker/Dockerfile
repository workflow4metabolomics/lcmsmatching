FROM ubuntu:14.04
#FROM ansible/ubuntu14.04-ansible:stable

MAINTAINER Pierrick Roger (pierrick.roger@gmail.com)

# Update package database
RUN apt-get update

# Add apt-add-repository tool
#RUN apt-get install --no-install-recommends -y software-properties-common

# Install Ansible
#RUN apt-add-repository ppa:ansible/ansible
#RUN apt-get update
#RUN apt-get install -y ansible
RUN apt-get install -y git
RUN git clone git://github.com/ansible/ansible.git --recursive
WORKDIR ./ansible
RUN apt-get install -y python python-setuptools
RUN apt-get install -y gcc
RUN apt-get install -y python-dev
RUN apt-get install -y libgmp-dev
#RUN bash -c "source ./hacking/env-setup && easy_install pip && pip install paramiko PyYAML Jinja2 httplib2 six && make all install"
RUN easy_install pip
RUN pip install paramiko PyYAML Jinja2 httplib2 six
RUN apt-get install -y libffi-dev
RUN apt-get install -y libssl-dev
RUN pip install 'requests[security]'
RUN apt-get install -y make
RUN make all install
RUN ansible --version
RUN mkdir /etc/ansible
RUN echo '[local]\nlocalhost\n' > /etc/ansible/hosts

# Run ansible role
ADD ansible /files/ansible
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
# TODO Make possible the two possibilities (either use local archive, or github repos)
#ADD w4m-tool-lcmsmatching-2.0.tar.gz /files/lcmsmatching/
#RUN apt-get install -y git
#WORKDIR /files
#RUN git clone --recursive https://github.com/pierrickrogermele/lcmsmatching 
#RUN R -e "install.packages('RJSONIO', lib='/usr/lib/R/library', dependencies = TRUE, repos='http://lib.ugent.be/CRAN/')"

# Clean up
RUN apt-get clean && apt-get autoremove -y && rm -rf /var/lib/{apt,dpkg,cache,log}/ /tmp/* /var/tmp/*

# Define Entry point script
#RUN chmod +x /files/lcmsmatching/search-mz
#ENTRYPOINT ["/files/lcmsmatching/search-mz"]
#ENTRYPOINT ["/files/lcmsmatching/r-msdb/search-mz"]
ENTRYPOINT ["/usr/bin/R"]
