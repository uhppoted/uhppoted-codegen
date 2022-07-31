package main

import (
	"fmt"
    "log"

    "uhppoted/uhppote"
)

func main() {
	fmt.Printf("uhppoted-codegen: Go test\n")

    if err := uhppote.GetDevices(); err != nil {
        log.Fatalf("ERROR  %v",err)
    }

    // request := uhppote.GetDevices()
    // fmt.Printf("%v\n", request)

    // reply := uhppote.Send(request)
    // fmt.Printf("%v\n", reply)
}
