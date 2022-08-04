package main

import (
	"fmt"
	"log"

	"uhppoted/uhppote"
)

func main() {
	fmt.Printf("uhppoted-codegen: Go test\n")

	if err := uhppote.GetAllControllers(); err != nil {
		log.Fatalf("ERROR  %v", err)
	}

	if err := uhppote.GetController(405419896); err != nil {
		log.Fatalf("ERROR  %v", err)
	}
}
