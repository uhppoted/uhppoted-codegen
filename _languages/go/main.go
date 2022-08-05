package main

import (
	"fmt"
	"log"

	"uhppoted/uhppote"
)

const ANY = "0.0.0.0:0"
const BROADCAST = "192.168.1.255:60000"

func main() {
	fmt.Printf("uhppoted-codegen: Go test\n")

	uhppote.SetBindAddr(ANY)
	uhppote.SetDestAddr(BROADCAST)

	if response, err := uhppote.GetAllControllers(); err != nil {
		log.Fatalf("ERROR  %v", err)
	} else {
		for _, v := range response {
			log.Printf("INFO  %+v", *v)
		}
	}

	if response, err := uhppote.GetController(405419896); err != nil {
		log.Fatalf("ERROR  %v", err)
	} else if response == nil {
		log.Fatalf("ERROR  %v", response)
	} else {
		log.Printf("INFO  %+v", *response)
	}
}
