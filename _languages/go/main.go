package main

import (
	"fmt"
	"log"

	"uhppoted/uhppote"
)

func main() {
	fmt.Printf("uhppoted-codegen: Go test\n")

	if response, err := uhppote.GetAllControllers(); err != nil {
		log.Fatalf("ERROR  %v", err)
	} else {
		for _,v := range response {
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
