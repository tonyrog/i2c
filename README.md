I2c linux interface
===================

# Raspberry pi
comment (enable) the blacklist entry in 
/etc/modprove.d/raspo-blacklist.conf 
it should read:

    # blacklist i2c-bcm2708

# Install the i2c tools
sudo apt-get install i2c-tools

# load the driver
sudo modprobe i2c-dev

# test the driver - example, list the i2c supported functions
sudo i2cdetect -F 0


# Experiments without hardware

modprobe i2c-stub


