package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/netip"

	"uhppoted/uhppote"
)

const ANY = "0.0.0.0:0"
const BROADCAST = "255.255.255.255:60000"

var ADDRESS = netip.MustParseAddr("192.168.1.100")
var NETMASK = netip.MustParseAddr("255.255.255.0")
var GATEWAY = netip.MustParseAddr("192.168.1.1")

type command struct {
	name string
	f    func() (any, error)
}

var commands = []command{
	command{
		name: "get-all-controllers",
		f: func() (any, error) {
			return uhppote.GetAllControllers()
		}},

	command{
		name: "get-controller",
		f: func() (any, error) {
			return uhppote.GetController(405419896)
		}},

	command{
		name: "set-ip",
		f: func() (any, error) {
			if err := uhppote.SetIP(405419896, ADDRESS, NETMASK, GATEWAY); err != nil {
				return nil, err
			}

			return nil, nil
		}},
}

func main() {
	fmt.Printf("uhppoted-codegen: Go sample application\n")

	options := struct{
		bind string
		broadcast string
		debug bool
	}{
		bind: ANY,
		broadcast: BROADCAST,
		debug: false,
	}

	flag.StringVar(&options.bind, "bind", options.bind, "UDP IPv4 bind address. Defaults to 0.0.0.0:0")
	flag.StringVar(&options.broadcast, "broadcast", options.broadcast, "UDP IPv4 broadcast address. Defaults to 255.255.255.255:60000")
	flag.BoolVar(&options.debug, "debug", options.debug, "Displays sent and received UDP packets")

	flag.Parse()

	if len(flag.Args()) == 0 {
		usage()
		return
	}

	uhppote.SetBindAddr(options.bind)
	uhppote.SetDestAddr(options.broadcast)
	uhppote.SetDebug(options.debug)

	list := flag.Args()
	for _, cmd := range list {
		for _, c := range commands {
			if c.name == cmd {
				if response, err := c.f(); err != nil {
					log.Fatalf("ERROR  %v", err)
				} else if response != nil {
					log.Printf("INFO  %+v", pprint(response))
				} else {
					log.Printf("WARN  no response")
				}
			}
		}
	}
}

func usage() {
	fmt.Println()
	fmt.Println("  Usage: go run main.go [--debug] [--bind <address>] [--broadcast <address>] [commands]")
	fmt.Println()
	fmt.Println("    Options:")
    fmt.Println("    --debug                Displays sent and received UDP packets");
    fmt.Println("    --bind <address>       IPv4 address to which to bind. Defaults to 0.0.0.0:0");
    fmt.Println("    --broadcast <address>  IPv4 address to which for UDP broadcast. Defaults to 255.255.255.255:60000");
	fmt.Println()
	fmt.Println("    Commands:")

	for _, c := range commands {
		fmt.Printf("      %v\n", c.name)
	}

	fmt.Println()
}

func pprint(v any) string {
	if bytes, err := json.MarshalIndent(v, "", "  "); err != nil {
		return fmt.Sprintf("%v", v)
	} else {
		return string(bytes)
	}
}
