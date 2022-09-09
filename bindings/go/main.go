package main

import (
	"flag"
	"fmt"
	"os"

	"golang.org/x/exp/slices"

	"uhppoted/uhppote"
)

const ANY = "0.0.0.0:0"
const BROADCAST = "255.255.255.255:60000"
const LISTEN = "0.0.0.0:60001"

func main() {
	fmt.Printf("uhppoted-codegen: Go sample application\n")

	options := struct {
		bind      string
		broadcast string
		listen string
		debug     bool
	}{
		bind:      ANY,
		broadcast: BROADCAST,
		listen: LISTEN,
		debug:     false,
	}

	flag.StringVar(&options.bind, "bind", options.bind, "UDP IPv4 bind address. Defaults to 0.0.0.0")
	flag.StringVar(&options.broadcast, "broadcast", options.broadcast, "UDP IPv4 broadcast address. Defaults to 255.255.255.255:60000")
	flag.StringVar(&options.listen, "listen", options.listen, "UDP IPv4 listen address. Defaults to 0.0.0.0:60001")
	flag.BoolVar(&options.debug, "debug", options.debug, "Displays sent and received UDP packets")
	flag.Parse()

	if len(flag.Args()) == 0 {
		usage()
		os.Exit(1)
	}

	uhppote.SetBindAddr(options.bind)
	uhppote.SetDestAddr(options.broadcast)
	uhppote.SetListenAddr(options.listen)
	uhppote.SetDebug(options.debug)

	list := flag.Args()

	if len(list) == 1 && list[0] == "all" {
		for _, c := range commands {
			if c.name != "listen" {
				c.exec()
			}
		}
	}else {
		for _, cmd := range list {
			ix := slices.IndexFunc[command](commands, func(c command) bool { return c.name == cmd })
			if ix != -1 {
				commands[ix].exec()
			} else {
				fmt.Println()
				fmt.Printf("   *** ERROR: invalid command %v\n", cmd)
				fmt.Println()

				os.Exit(1)
			}
		}
	}

	os.Exit(0)
}

func usage() {
	fmt.Println()
	fmt.Println("  Usage: go run main.go [--debug] [--bind <address>] [--broadcast <address>] [commands]")
	fmt.Println()
	fmt.Println("    Options:")
	fmt.Println("    --debug                Displays sent and received UDP packets")
	fmt.Println("    --bind <address>       IPv4 address to which to bind. Defaults to 0.0.0.0")
	fmt.Println("    --broadcast <address>  IPv4 address to which for UDP broadcast. Defaults to 255.255.255.255:60000")
	fmt.Println()
	fmt.Println("    Commands:")

	for _, c := range commands {
		fmt.Printf("      %v\n", c.name)
	}

	fmt.Println()
}