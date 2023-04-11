.PHONY: run

debug: 
	php uhppoted.php --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 --debug

usage: 
	php uhppoted.php --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 --debug

all:
	php uhppoted.php --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 --debug all

get-all-controllers: 
#	php uhppoted.php get-all-controllers
	php uhppoted.php --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 --debug get-all-controllers

get-controller: 
	php uhppoted.php --debug get-controller

set-ip: 
	php uhppoted.php --debug set-ip


listen: 
	php uhppoted.php listen
#	php uhppoted.php --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 --debug listen
