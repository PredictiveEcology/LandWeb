mkdir -p /mnt/shared/LandWeb/cache
mkdir -p /mnt/shared/LandWeb/outputs
mkdir -p /mnt/shared/LandWeb/www

sudo groupadd landweb
sudo usermod -a -G landweb achubaty
sudo usermod -a -G landweb emcintir
sudo chown -R :landweb /mnt/shared/LandWeb
sudo chmod -R g+w /mnt/shared/LandWeb

################################################################################
# do the following as eliot:
sudo su emcintir

## cache/
mv /home/emcintir/Documents/GitHub/LandWeb/cache /mnt/shared/LandWeb/
ln -s /mnt/shared/LandWeb/cache ~/Documents/GitHub/LandWeb/cache

## outputs/
mv /home/emcintir/Documents/GitHub/LandWeb/outputs /mnt/shared/LandWeb/
ln -s /mnt/shared/LandWeb/outputs ~/Documents/GitHub/LandWeb/outputs

## www/
mv ~/Documents/GitHub/LandWeb/www /mnt/shared/LandWeb/
ln -s /mnt/shared/LandWeb/www ~/Documents/GitHub/LandWeb/www

exit
################################################################################

sudo chown -R :landweb /mnt/shared/LandWeb
sudo chmod -R g+w /mnt/shared/LandWeb

## backup LandWeb outputs on 342
rsync -avP /mnt/shared/LandWeb/ /mnt/data/LandWeb-backup/
