# Deploying LandWeb app to LandWeb.ca

## Server information

landweb.ca (104.37.196.228)

**Details:**

- *CPUs*: 6
- *RAM:* 32 GB
- *Storage:* 60GB (OS drive; SSD)
- *Operating system:* Ubuntu 16.04 LTS (user `ubuntu`)

**Contact:**

Bryan L (<bryan@bigpixel.ca>).

## Ubuntu setup

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

### Install nodejs + mapshaper

```bash
## the commands below work for Ubuntu 16.04+;
## for Debian, also need https://github.com/nodejs/help/issues/1040#issuecomment-362970187
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt-get install -y nodejs

sudo npm install -g mapshaper
```

### Install R

```bash
# Install R
sudo apt update
#sudo apt -y install nginx  ## why do we need this??

sudo sh -c 'echo "deb https://cran.rstudio.com/bin/linux/ubuntu bionic-cran35/" >> /etc/apt/sources.list.d/cran.list'
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo apt update
sudo apt -y install r-base
sudo apt -y install r-base-dev

## add repo for GIS tools
sudo add-apt-repository ppa:ubuntugis/ppa
sudo apt update
```

Check installed R version:

```bash
Rscript -e 'sessionInfo()'
```

### Install shiny-server

```bash
## TODO: update this
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
wget https://download2.rstudio.org/server/trusty/amd64/rstudio-server-1.2.1335-amd64.deb
sudo gdebi rstudio-server-1.2.1335-amd64.deb
```

### Install additional R package dependencies

```bash
sudo apt build-dep -y r-cran-rjava r-cran-tkrplot
sudo R CMD javareconf

sudo apt install -y \
    gdal-bin \
    git \
    libcairo2-dev \
    libcurl4-gnutls-dev \
    libgdal-dev \
    libgeos-dev \
    libgit2-dev \
    libgmp-dev \
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

#sudo add-apt-repository -y ppa:opencpu/jq ## no longer needed in bionic?
#sudo apt update
#sudo apt install -y libjq-dev

# Make /usr/lib/R/site-library owned by user
sudo chown -R ubuntu:ubuntu /usr/local/lib/R/site-library
sudo chmod 775 -R /usr/local/lib/R/site-library
```

### Install additonal R packages

#### Setup package caching

Cache package compilation to speed up future package updates/reinstallations:

See http://dirk.eddelbuettel.com/blog/2017/11/27/#011_faster_package_installation_one.

```bash
sudo apt install -y ccache

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
  "rmapshaper",
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

deps <- TRUE
devtools::install_github("PredictiveEcology/SpaDES.core", ref = "development", dependencies = deps)
devtools::install_github("PredictiveEcology/pemisc", ref = "development", dependencies = deps)
devtools::install_github("PredictiveEcology/map", ref = "development", dependencies = deps)
devtools::install_github("PredictiveEcology/LandR", ref = "development", dependencies = deps)
devtools::install_github("PredictiveEcology/LandWebUtils", ref = "development", dependencies = deps)
devtools::install_github("achubaty/amc", ref = "development", dependencies = deps)
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

#### App setup

### Copy app files

From GitHub:

```bash
su achubaty
git clone --recursive -j8 git@github.com:eliotmcintire/LandWeb.git /tmp/LandWeb
cd /tmp/LandWeb
git checkout development ## TODO: should be working from master branch once merged
exit

whoami # should now be user `ubuntu`
cd /srv/shiny-server
sudo mv /tmp/LandWeb /srv/shiny-server/

cd /srv/shiny-server/LandWeb
sudo cp global_file.R global.R
sudo mkdir /srv/shiny-server/cache
sudo mkdir /srv/shiny-server/outputs
sudo mkdir /srv/shiny-server/www/All
sudo mkdir /srv/shiny-server/www/Free
sudo mkdir /srv/shiny-server/www/Proprietary
```

From 342:

```bash
##  cache/
rsync -ruvzP --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/cache/FULL ubuntu@landweb.ca:/srv/shiny-server/LandWeb/cache/
rsync -ruvzP --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/cache/FULL_All ubuntu@landweb.ca:/srv/shiny-server/LandWeb/cache/
#rsync -ruvzP --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/cache/FULL_Proprietary ubuntu@landweb.ca:/srv/shiny-server/LandWeb/cache/

## inputs/
rsync -ruvzP --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/m landweb:/srv/shiny-server/LandWeb/

##  outputs/
rsync -ruvzP --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/outputs/FULL ubuntu@landweb.ca:/srv/shiny-server/LandWeb/outputs/
rsync -ruvzP --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/outputs/FULL_All ubuntu@landweb.ca:/srv/shiny-server/LandWeb/outputs/
#rsync -ruvzP --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/outputs/FULL_Proprietary ubuntu@landweb.ca:/srv/shiny-server/LandWeb/outputs/

##  www/
rsync -ruvzP --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i ~/.ssh/id_rsa_landweb" ~/Documents/GitHub/LandWeb/www/ ubuntu@landweb.ca:/srv/shiny-server/LandWeb/
```

Make sure permissions are correct:

```bash
sudo chown -R achubaty:achubaty /srv/shiny-server/LandWeb
```

#### Keep a working copy

Allows development etc. in Rstudio.

```bash
su- achubaty
cd ~/Documents/GitHub/

# git clone git@github.com:PredictiveEcology/quickPlot.git
# git clone git@github.com:PredictiveEcology/reproducible.git
# git clone git@github.com:PredictiveEcology/SpaDES.git
# git clone git@github.com:PredictiveEcology/SpaDES.core.git
# git clone git@github.com:PredictiveEcology/SpaDES.tools.git

git clone --recursive -j8 git@github.com:eliotmcintire/LandWeb.git
git checkout development
#git submodule update --init --recursive

cd ~/GitHub/LandWeb
ln -s /srv/shiny-server/LandWeb/cache cache
ln -s /srv/shiny-server/LandWeb/outputs outputs
ln -s /srv/shiny-server/LandWeb/www/All www/All
ln -s /srv/shiny-server/LandWeb/www/Free www/Free
ln -s /srv/shiny-server/LandWeb/www/Proprietary www/Proprietary
```

### Additional config

#### shiny server

```bash
sudo nano /etc/shiny-server/shiny-server.conf
```

Need to edit a few things in `/etc/shiny-server/shiny-server.conf`, as follows.

Outside the server block:

```
run_as achubaty  ## instead of user `shiny`
```

In the server block, edit accordingly:

```
server_name landweb.ca
listen 80;
```

In the server's location block, add:

```
# Application timeouts
app_idle_timeout 86400; # 24 hours
app_init_timeout 1800;  # 30 mins

# Google Analytics
google_analytics_id UA-119802371-1;


app_dir /srv/shiny-server/LandWeb; ## use app_dir instead of site_dir
```

Once shiny-server is up and running:

```bash
sudo rm -rf /srv/shiny-server/sample-apps
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
