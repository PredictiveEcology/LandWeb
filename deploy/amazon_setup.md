# Deploying LandWeb app to Amazon

## Instance information

**Size:** Medium
**Domain:**: ec2-52-26-180-235.us-west-2.compute.amazonaws.com
**IP:** 52.26.180.235

**Size:** Small
**Domain:** ec2-52-89-34-122.us-west-2.compute.amazonaws.com
**IP:** 52.89.34.122

## Ubuntu setup

### Tutorials

1. https://aws.amazon.com/getting-started/launch-a-virtual-machine-B-0/launch-a-virtual-machine-B-1/

2. https://www.r-bloggers.com/shiny-server-on-aws/

3. http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/managing-users.html

4. http://www.tekgoblin.com/2013/04/29/aws-guides-how-to-increase-your-ec2-linux-root-volume-size/

### System setup

```bash
# set up ubuntu unattended upgrades
sudo apt-get install unattended-upgrades
sudo dpkg-reconfigure unattended-upgrades

# add users
ssh -i .ssh/laptopTesting.pem ubuntu@ec2-52-26-180-235.us-west-2.compute.amazonaws.com
ssh -i .ssh/laptopTesting.pem achubaty@ec2-52-26-180-235.us-west-2.compute.amazonaws.com
sudo adduser emcintir
sudo adduser emcintir sudo
sudo adduser emcintir shiny
sudo su - emcintir
mkdir .ssh
chmod 700 .ssh
touch .ssh/authorized_keys
chmod 600 .ssh/authorized_keys
nano .ssh/authorized_keys

sudo adduser achubaty
sudo adduser achubaty sudo
sudo adduser achubaty shiny
sudo su - achubaty
mkdir .ssh
chmod 700 .ssh
touch .ssh/authorized_keys
chmod 600 .ssh/authorized_keys
nano .ssh/authorized_keys

# Get public key value --- then copy this into authorized_keys on Amazon server
ssh-keygen -y

# Disconnect ubuntu user and reconnect as emcintir
exit
```

### Install R

```bash
ssh -i .ssh/laptopTesting.pem emcintir@ec2-52-26-180-235.us-west-2.compute.amazonaws.com

# Install R
sudo apt-get update
sudo apt-get -y install nginx

sudo sh -c 'echo "deb https://cran.rstudio.com/bin/linux/ubuntu xenial/" >> /etc/apt/sources.list'
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo apt-get update
sudo apt-get -y install r-base
sudo apt-get -y install r-base-dev

R
> sessionInfo()
> q()

sudo apt-get -y install gdebi-core
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
sudo gdebi shiny-server-1.5.1.834-amd64.deb

sudo adduser emcintir shiny
```

### Install rstudio-server

```bash
# Rstudio
sudo apt-get install gdebi-core
wget https://download2.rstudio.org/rstudio-server-1.1.442-amd64.deb
sudo gdebi rstudio-server-1.1.442-amd64.deb

# Other things
sudo apt-get install -y \
    pandoc \
    pandoc-citeproc \
    libssl-dev \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libxml2-dev \
    libxt-dev \
    libv8-dev \
    git


# Make /usr/lib/R/site-library owned by emcintir
sudo chown -R emcintir:emcintir /usr/local/lib/R/site-library
sudo chmod 775 -R /usr/local/lib/R/site-library

pkgNamespaces <- c("htmlwidgets", "shiny", "shinydashboard", "shinyBS", "leaflet",
                     "BH", "RCurl", "RandomFieldsUtils", "R.oo", "R.methodsS3", "SpaDES", "markdown",
                     "visNetwork", "rgexf", "influenceR", "DBI", "viridis", "bit", "parallel",
                     "devtools", "raster", #"rgeos",
					 "RSQLite", "magrittr", "raster", #"sp",
                     "maptools", "broom", "ggvis", #"rgdal",
					 "grid", "VGAM", "rmarkdown")
  lapply(pkgNamespaces, function(p) if (!require(p, quietly = TRUE, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE, lib="/usr/local/lib/R/site-library",
	repos='http://cran.rstudio.com/')
  })
  if (!require("RandomFieldsUtils", character.only = TRUE)) install.packages("RandomFieldsUtils")
  if (!require("RandomFields", character.only = TRUE)) install.packages("RandomFields")

sudo apt-get install r-cran-tkrplot

sudo chown -R emcintir:emcintir /usr/local/lib/R/site-library
sudo chmod 775 -R /usr/local/lib/R/site-library
```

### GitHub config

```bash
# Make directory for receiving app data
mkdir -p ~/Documents/GitHub/

# Connect to github
ssh-keygen -t rsa -b 4096 -C "eliotmcintire@gmail.com"
eval $(ssh-agent -s)
ssh-add ~/.ssh/id_rsa
cat ~/.ssh/id_rsa.pub

# clone repository
cd ~/Documents/GitHub/
git clone git@github.com:eliotmcintire/LandWeb.git
git clone git@github.com:PredictiveEcology/quickPlot.git
git clone git@github.com:PredictiveEcology/reproducible.git
git clone git@github.com:PredictiveEcology/SpaDES.git

## ssh
ssh -i .ssh/laptopTesting.pem emcintir@ec2-52-26-180-235.us-west-2.compute.amazonaws.com
```

### Copy app data/outputs to new server

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
## Restart shiny server
sudo service shiny-server restart

## Shiny config file
sudo nano /etc/shiny-server/shiny-server.conf

## Need to edit a few things in the shiny-server.conf
app_idle_timeout 24000; # 6 hours
listen 80;
server_name landweb.predictiveecology.org

### To 388 as a server
ssh emcintir@132.156.149.44

### Configure -- for security from 
### https://www.thefanclub.co.za/how-to/how-secure-ubuntu-1604-lts-server-part-1-basics

sudo service shiny-server stop
sudo apt-get install ufw # will turn on nginx
sudo ufw allow ssh
sudo ufw allow http
sudo ufw allow 8787/tcp # all for Rstudio server
sudo service nginx stop
sudo service shiny-server start

sudo apt-get install fail2ban

# configure fali2ban
sudo nano /etc/fail2ban/jail.conf
sudo service fail2ban restart

## rsync 388 directly to 342
rsync -ruv --exclude '.git' --exclude '.Rproj.user' --exclude '.checkpoint' --delete -e "ssh -i .ssh/laptopTesting.pem" ~/Documents/GitHub/LandWeb/ emcintir@ec2-52-26-180-235.us-west-2.compute.amazonaws.com:/srv/shiny-server/Demo/

## create symlinks
rm -r /home/emcintir/Documents/GitHub/LandWeb

ln -s /srv/shiny-server/Demo/ /home/emcintir/Documents/GitHub/LandWeb
ln -s /srv/shiny-server/Demo2/ /home/emcintir/Documents/GitHub/LandWeb
#ln -sf /srv/shiny-server/Demo2 /home/emcintir/Documents/GitHub/LandWeb

# move from Demo2 to Demo
sudo mkdir /srv/shiny-server/Demo3

sudo mv /srv/shiny-server/Demo/* /srv/shiny-server/Demo3/
sudo mv /srv/shiny-server/Demo2/* /srv/shiny-server/Demo/
```
