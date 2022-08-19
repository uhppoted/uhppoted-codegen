package main

import (
    "encoding/json"
    "fmt"
    "log"
    "net/netip"
    "time"

    "uhppoted/uhppote"
)

const ANY = "0.0.0.0:0"
const BROADCAST = "255.255.255.255:60000"
const CONTROLLER uint32 = 405419896

var ADDRESS = netip.MustParseAddr("192.168.1.100")
var NETMASK = netip.MustParseAddr("255.255.255.0")
var GATEWAY = netip.MustParseAddr("192.168.1.1")

type command struct {
    name string
    f    func() (any, error)
}

var commands = []command{
    command{name: "get-all-controllers",f:    getAllControllers},
    command{ name: "get-controller",f:    getController },
    command{ name: "set-ip", f:    setIP},
    command{ name: "get-time", f:    getTime},
    command{name: "set-time", f:    setTime},
    command{ name: "get-status", f:    getStatus},
    command{ name: "get-listener", f:    getListener},
}

func (c command) exec() {
    if response, err := c.f(); err != nil {
        log.Fatalf("ERROR  %v", err)
    } else if response != nil {
        log.Printf("INFO  %+v", pprint(response))
    } else {
        log.Printf("WARN  no response")
    }
}

func pprint(v any) string {
    if bytes, err := json.MarshalIndent(v, "", "  "); err != nil {
        return fmt.Sprintf("%v", v)
    } else {
        return string(bytes)
    }
}

func getAllControllers() (any, error) {
    return uhppote.GetAllControllers()
}

func getController() (any, error) {
    return uhppote.GetController(CONTROLLER)
}

func setIP() (any, error) {
    if err := uhppote.SetIP(CONTROLLER, ADDRESS, NETMASK, GATEWAY); err != nil {
        return nil, err
    }

    return nil, nil
}

func getTime() (any, error) {
    return uhppote.GetTime(CONTROLLER)
}

func setTime() (any, error) {
    return uhppote.SetTime(CONTROLLER, uhppote.DateTime(time.Now()))
}

func getStatus() (any, error) {
    return uhppote.GetStatus(CONTROLLER)
}

func getListener() (any, error) {
    return uhppote.GetListener(CONTROLLER)
}
