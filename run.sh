#! /bin/bash

sudo iptables -t nat -I OUTPUT -p tcp -d 127.0.0.1 --dport 80 -j REDIRECT --to-ports 8080
sudo iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 8080
export PYTHON_VLC_LIB_PATH=/usr/lib/arm-linux-gnueabihf/libvlc.so.5
cd /home/pi/podcastpy
/home/pi/.local/bin/pipenv run pserve /home/pi/podcastpy/production.ini
