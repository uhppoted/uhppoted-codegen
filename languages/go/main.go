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
const BROADCAST = "192.168.1.255:60000"

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

	flag.Parse()

	if len(flag.Args()) == 0 {
		usage()
		return
	}

	uhppote.SetBindAddr(ANY)
	uhppote.SetDestAddr(BROADCAST)

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
	fmt.Println("  Usage: go run main.go [commands]")
	fmt.Println()
	fmt.Println("    Supported commands:")

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
