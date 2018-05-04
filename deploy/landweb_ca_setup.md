# Deploying LandWeb app to LandWeb.ca

## Server information

landweb.ca (104.37.196.228)

## Ubuntu setup

Ubuntu 16.04 was pre-installed with user `ubuntu`.

### System setup

```bash
# set up ubuntu unattended upgrades
sudo apt install unattended-upgrades
sudo dpkg-reconfigure unattended-upgrades

sudo apt install htop
```

### Install R

```bash
# Install R
sudo apt update
#sudo apt -y install nginx  ## why do we need this??

sudo sh -c 'echo "deb https://cran.rstudio.com/bin/linux/ubuntu xenial/" >> /etc/apt/sources.list.d/cran.list'
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo apt update
sudo apt -y install r-base
sudo apt -y install r-base-dev

## install GIS tool
sudo add-apt-repository ppa:ubuntugis/ppa
sudo apt update

```

Check installed R version:
```bash
Rscript -e 'sessionInfo()'
```

### Install shiny-server

```bash
sudo apt -y install gdebi-core
sudo su - -c "R -e \"install.packages('shiny', repos = 'http://cran.rstudio.com/')\""
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.7.907-amd64.deb
sudo gdebi shiny-server-1.5.7.907-amd64.deb

# add user to the shiny group
sudo adduser ubuntu shiny
```

### Install rstudio-server

```bash
# Rstudio
sudo apt -y install gdebi-core
wget https://download2.rstudio.org/rstudio-server-1.1.447-amd64.deb
sudo gdebi rstudio-server-1.1.447-amd64.deb
```

### Install additional R package dependencies

```bash
sudo apt build-dep r-cran-rjava
sudo R CMD javareconf

sudo apt install -y \
    gdal-bin \
    git \
    libcairo2-dev \
    libcurl4-gnutls-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libprotobuf-dev \
    libprotoc-dev \
    libssl-dev \
    libudunits2-dev \
    libv8-dev \
    libxml2-dev \
    libxt-dev \
    pandoc \
    pandoc-citeproc \
    protobuf-compiler \
    python-gdal \
    python3-gdal \
    r-cran-tkrplot

sudo add-apt-repository -y ppa:opencpu/jq
sudo apt update
sudo apt install libjq-dev

# Make /usr/lib/R/site-library owned by user
sudo chown -R ubuntu:ubuntu /usr/local/lib/R/site-library
sudo chmod 775 -R /usr/local/lib/R/site-library
```

### Install additonal R packages

#### Setup package caching

Cache package compilation to speed up future package updates/reinstallations:

See http://dirk.eddelbuettel.com/blog/2017/11/27/#011_faster_package_installation_one.

```bash
sudo apt install ccache

mkdir -p ~/.ccache
mkdir -p ~/.R
```

Add the following to `~/.R/Makevars`:

```
VER=
CCACHE=ccache
CC=$(CCACHE) gcc$(VER)
CXX=$(CCACHE) g++$(VER)
CXX11=$(CCACHE) g++$(VER)
CXX14=$(CCACHE) g++$(VER)
FC=$(CCACHE) gfortran$(VER)
F77=$(CCACHE) gfortran$(VER)
```

Add the following to `~/.ccache/ccache.conf`:

```
max_size = 5.0G
# important for R CMD INSTALL *.tar.gz as tarballs are expanded freshly -> fresh ctime
sloppiness = include_file_ctime
# also important as the (temp.) directory name will differ
hash_dir = false
```

#### R package installation

```r
pkgs <- c(
  "BH",
  "bit",
  "broom",
  "DBI",
  "devtools",
  "ggvis",
  "htmlwidgets",
  "influenceR",
  "leaflet",
  "magrittr",
  "maptools",
  "markdown",
  "shiny",
  "shinyBS",
  "shinydashboard",
  "shinyjs",
  "R.methodsS3",
  "R.oo",
  "RandomFieldsUtils",
  "raster",
  "RCurl",
  "rgdal",
  "rgeos",
  "rgexf",
  "rJava",
  "rmarkdown",
  "RSQLite",
  "sf",
  "sp",
  "SpaDES",
  "VGAM",
  "viridis",
  "visNetwork"
)

notInstalled <- vapply(pkgs, function(p) {
  !require(p, quietly = TRUE, character.only = TRUE)
}, logical(1))
pkgs2install <- pkgs[notInstalled]
install.packages(pkgs2install, dependencies = TRUE, lib = .Library.site[1], repos = "https://cran.rstudio.com/")
```

### GitHub config

```bash
# Make directory for receiving app data
mkdir -p ~/Documents/GitHub/

# Connect to github
ssh-keygen -t rsa -b 4096 -C "landweb@landweb.ca"
eval $(ssh-agent -s)
ssh-add ~/.ssh/id_rsa
cat ~/.ssh/id_rsa.pub

# clone repository
cd ~/Documents/GitHub/
git clone git@github.com:eliotmcintire/LandWeb.git
# git clone git@github.com:PredictiveEcology/quickPlot.git
# git clone git@github.com:PredictiveEcology/reproducible.git
# git clone git@github.com:PredictiveEcology/SpaDES.git
# git clone git@github.com:PredictiveEcology/SpaDES.core.git
# git clone git@github.com:PredictiveEcology/SpaDES.tools.git
```

## Copy app data/outputs to new server

```bash
## rsync 388 directly to /srv/shiny-server & ssh
rsync -ruv --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i .ssh/laptopTesting.pem" ~/Documents/GitHub/LandWeb/ emcintir@ec2-52-26-180-235.us-west-2.compute.amazonaws.com:/srv/shiny-server/Demo2/
ssh -t -i .ssh/laptopTesting.pem emcintir@ec2-52-26-180-235.us-west-2.compute.amazonaws.com 'sudo chown -R shiny:shiny /srv/shiny-server/Demo/. && sudo chmod 775 -R /srv/shiny-server/Demo/.'

ssh -t -i .ssh/laptopTesting.pem emcintir@ec2-52-26-180-235.us-west-2.compute.amazonaws.com 'sudo service shiny-server restart'

## rsync on AWS between two locations
rsync -ruv --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e /srv/shiny-server/Demo/ /srv/shiny-server/Main/

# or log onto remote Amazon
ssh -i .ssh/laptopTesting.pem emcintir@ec2-52-26-180-235.us-west-2.compute.amazonaws.com
sudo mkdir /srv/shiny-server/Demo2
sudo chown -R shiny:shiny /srv/shiny-server/Demo2/.
sudo chmod 775 -R /srv/shiny-server/Demo2/.
sudo chmod g+s -R /srv/shiny-server/Demo2/.

sudo chown -R shiny:shiny /home/emcintir/Documents/GitHub/LandWeb/.
sudo chmod 775 -R /home/emcintir/Documents/GitHub/LandWeb/.
sudo chmod g+s -R /home/emcintir/Documents/GitHub/LandWeb/.
```

### Additional config

```bash
## shiny config file
sudo nano /etc/shiny-server/shiny-server.conf

## Need to edit a few things in the shiny-server.conf
app_idle_timeout 24000; # 6 hours
listen 80;
server_name landweb.ca

## Restart shiny server
sudo systemctl restart shiny-server.service

### Configure -- for security from
### https://www.thefanclub.co.za/how-to/how-secure-ubuntu-1604-lts-server-part-1-basics

sudo systemctl stop shiny-server.service

#sudo apt install ufw ## already installed and configured to allow https and https traffic
sudo ufw allow ssh
#sudo ufw allow http
#sudo ufw allow https
sudo ufw allow 8787/tcp # all for Rstudio server

sudo systemctl stop shiny-server.service

## restart rstudio server
sudo systemctl restart rstudio-server.service

sudo apt install fail2ban

# configure fail2ban
sudo nano /etc/fail2ban/jail.conf
sudo service fail2ban restart

## rsync 388 directly to 342
rsync -ruvzP --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/ ubuntu@landweb.ca:/srv/shiny-server/Landweb/

## create symlinks
rm -r /home/emcintir/Documents/GitHub/LandWeb
ln -s /srv/shiny-server/LandWeb/ /home/ubuntu/Documents/GitHub/LandWeb
```
