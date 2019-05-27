#!/bin/bash
sudo apt-get install unattended-upgrades xvfb xauth xfonts-base
sudo dpkg-reconfigure unattended-upgrades

sudo apt-get install htop

# Install R
sudo apt-get update
#sudo apt-get -y install nginx  ## why do we need this??

sudo sh -c 'echo "deb https://cran.rstudio.com/bin/linux/ubuntu bionic-cran35/" >> /etc/apt/sources.list.d/cran.list'
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo apt-get update
sudo apt-get -y install r-base
sudo apt-get -y install r-base-dev

## add repo for GIS tools
sudo add-apt-repository ppa:ubuntugis/ppa
sudo apt-get update

## mapshaper
curl -sL https://deb.nodesource.com/setup_12.x | sudo -E bash -
sudo apt-get install -y nodejs
sudo npm install -g mapshaper

# Rstudio
sudo apt-get -y install gdebi-core
wget https://download2.rstudio.org/server/bionic/amd64/rstudio-server-1.2.1335-amd64.deb
sudo gdebi rstudio-server-1.2.1335-amd64.deb

sudo apt build-dep -y r-cran-rjava r-cran-tkrplot
sudo R CMD javareconf

sudo apt-get install -y \
    gdal-bin \
    git \
    libcairo2-dev \
    libcurl4-gnutls-dev \
    libgdal-dev \
    libgeos-dev \
    libgit2-dev \
    libmagick++-dev \
    libproj-dev \
    libprotobuf-dev \
    libprotoc-dev \
    libssh2-1-dev \
    libssl-dev \
    libudunits2-dev \
    libv8-dev \
    libxml2-dev \
    libxt-dev \
    pandoc \
    pandoc-citeproc \
    protobuf-compiler \
    python-gdal \
    python3-gdal

sudo apt-get install -y ccache
