mkdir -p /mnt/shared/LandWeb/outputs
mkdir -p /mnt/shared/LandWeb/cache
mkdir -p /mnt/shared/LandWeb/www

sudo groupadd landweb
sudo usermod -a -G landweb achubaty
sudo usermod -a -G landweb emcintir
sudo chown :landweb /mnt/shared/
sudo chown -R :landweb /mnt/shared/LandWeb

ln -s /mnt/shared/LandWeb/cache ~/Documents/GitHub/LandWeb/cache
ln -s /mnt/shared/LandWeb/outputs ~/Documents/GitHub/LandWeb/outputs
ln -s /mnt/shared/LandWeb/www ~/Documents/GitHub/LandWeb/www
