# Deploying LandWeb app to LandWeb.ca

## Server information

landweb.ca (104.37.196.228)

## Ubuntu setup

Ubuntu 16.04 was pre-installed with user `ubuntu`.

Create a new (non-admin) user for use with Rstudio:

```bash
sudo adduser achubaty
```

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

## requires older version of shiny (see # 32) [packrat could help here!]
if (packageVersion("shiny") > "1.0.5") {
  install.packages("https://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.0.5.tar.gz", repos = NULL)
}

devtools::install_github("PredictiveEcology/SpaDES.shiny@generalize-modules")
```

### GitHub config

```bash
# switch user
su achubaty
cd /home/achubaty

# git config
git config --global core.editor nano
git config --global user.name "LandWeb.ca"
git config --global user.email "alex.chubaty+landweb@gmail.com"

# Make directory for receiving app data
mkdir -p ~/Documents/GitHub/
```

#### SSH keys

Create ssh key for use with GitHub:

```bash
ssh-keygen -t rsa -b 4096 -C "alex.chubaty+landweb@gmail.com"
eval $(ssh-agent -s)
ssh-add ~/.ssh/id_rsa_github
cat ~/.ssh/id_rsa_github.pub
```

Copy the output and add the key at GitHub.com.
See https://help.github.com/articles/adding-a-new-ssh-key-to-your-github-account/.

#### SSH config

Edit `~/.ssh/config` to add:

```
Host github.com
	HostName github.com
	IdentityFile ~/.ssh/id_rsa_github
	User git
```

#### Repository setup

```bash
cd ~/Documents/GitHub/

# git clone git@github.com:PredictiveEcology/quickPlot.git
# git clone git@github.com:PredictiveEcology/reproducible.git
# git clone git@github.com:PredictiveEcology/SpaDES.git
# git clone git@github.com:PredictiveEcology/SpaDES.core.git
# git clone git@github.com:PredictiveEcology/SpaDES.tools.git

git clone --recurse-submodules git@github.com:eliotmcintire/LandWeb.git
git checkout reactive-sims
git submodule update --init --recursive
```

## Copy app data/outputs to new server

```bash
## rsync from 342 to landweb.ca
rsync -ruv --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/ ubuntu@landweb.ca:/srv/shiny-server/LandWeb/

ssh -t -i ~/.ssh/id_rsa_landweb ubuntu@landweb.ca 'sudo chown -R shiny:shiny /srv/shiny-server/LandWeb/. && sudo chmod 775 -R /srv/shiny-server/LandWeb/.'

ssh -t -i ~/.ssh/id_rsa_landweb ubuntu@landweb.ca 'sudo systemctl restart shiny-server.service'

# or log onto remote server
ssh -i ~/.ssh/id_rsa_landweb ubuntu@landweb.ca
sudo mkdir /srv/shiny-server/LandWeb
sudo chown -R shiny:shiny /srv/shiny-server/LandWeb
sudo chmod 775 -R /srv/shiny-server/LandWeb
sudo chmod g+s -R /srv/shiny-server/LandWeb
```

```bash
## rsync 388 directly to /srv/shiny-server & ssh
rsync -ruv --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/ ubuntu@landweb.ca:/srv/shiny-server/LandWeb/
```

### Additional config

#### shiny server

```bash
sudo nano /etc/shiny-server/shiny-server.conf
```

```
## Need to edit a few things in the shiny-server.conf
server_name landweb.ca
listen 80;

app_idle_timeout 24000; # 6 hours
google_analytics_id UA-119802371-1;
```

```bash
# once shiny-server is up and running:
sudo rm -rf /srv/shiny-server/sample-apps

# restart shiny server
sudo systemctl restart shiny-server.service
```
#### Rstudio server

```bash
sudo nano /etc/rstudio/rserver.conf
```

```
www-port=8787
```

```bash
## restart rstudio server
sudo rstudio-server restart
```

#### firewall & security

```bash
### Configure -- for security from
### https://www.thefanclub.co.za/how-to/how-secure-ubuntu-1604-lts-server-part-1-basics

#sudo apt install ufw ## already installed and configured to allow https and https traffic
sudo ufw allow ssh
#sudo ufw allow http
#sudo ufw allow https
sudo ufw allow 8787/tcp # all for Rstudio server

sudo apt install fail2ban

# configure fail2ban
sudo nano /etc/fail2ban/jail.conf
sudo service fail2ban restart
```

### Copy app files

```bash
## rsync directly from 342
rsync -ruvzP --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/cache/* ubuntu@landweb.ca:/home/ubuntu/Documents/GitHub/LandWeb/cache/
rsync -ruvzP --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/outputs/* ubuntu@landweb.ca:/home/ubuntu/Documents/GitHub/LandWeb/outputs/
rsync -ruvzP --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/www ubuntu@landweb.ca:/home/ubuntu/Documents/GitHub/LandWeb/

## create symlinks
rm -r /home/achubaty/Documents/GitHub/LandWeb
ln -s /srv/shiny-server/LandWeb/ /home/achubaty/Documents/GitHub/LandWeb
```
