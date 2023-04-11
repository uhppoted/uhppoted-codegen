.PHONY: run

debug: 
	php uhppoted.php --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 --debug

usage: 
	php uhppoted.php --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 --debug

all:
	php uhppoted.php --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 --debug all

get-all-controllers: 
#	php uhppoted.php --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 --debug get-all-controllers
	php uhppoted.php get-all-controllers --timeout=1 --debug 

get-controller: 
	php uhppoted.php --debug --timeout=1 get-controller

set-ip: 
	php uhppoted.php --debug --timeout=1  set-ip

listen: 
#	php uhppoted.php --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 --debug listen
	php uhppoted.php listen
