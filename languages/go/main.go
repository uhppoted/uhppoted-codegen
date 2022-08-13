package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"

	"uhppoted/uhppote"
)

const ANY = "0.0.0.0:0"
const BROADCAST = "192.168.1.255:60000"

type command struct {
	name string
	f    func() (any, error)
}

var commands = []command{
	command{"get-all-controllers", func() (any, error) {
		return uhppote.GetAllControllers()
	}},
	command{"get-controller", func() (any, error) {
		return uhppote.GetController(405419896)
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
				} else if response == nil {
					log.Fatalf("ERROR  %v", response)
				} else {
					log.Printf("INFO  %+v", pprint(response))
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
