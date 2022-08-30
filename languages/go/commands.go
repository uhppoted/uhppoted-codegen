package main

import (
    "encoding/json"
    "fmt"
    "log"
    "net/netip"
    "os"
    "os/signal"
    "syscall"
    "time"

    "uhppoted/uhppote"
)

const ANY = "0.0.0.0:0"
const BROADCAST = "255.255.255.255:60000"
const CONTROLLER uint32 = 405419896
const DOOR uint8 = 3
const MODE uint8 = 2
const DELAY uint8 = 10
const CARD uint32 = 8165538
const CARD_INDEX uint32 = 3
const EVENT_INDEX uint32 = 37
const TIME_PROFILE_ID uint8 = 29

var ADDRESS = netip.MustParseAddr("192.168.1.100")
var NETMASK = netip.MustParseAddr("255.255.255.0")
var GATEWAY = netip.MustParseAddr("192.168.1.1")
var LISTENER = netip.MustParseAddrPort("192.168.1.100:60001")

type command struct {
    name string
    f    func() (any, error)
}

var commands = []command{
    command{name: "get-all-controllers", f: getAllControllers},
    command{name: "get-controller", f: getController},
    command{name: "set-ip", f: setIP},
    command{name: "get-time", f: getTime},
    command{name: "set-time", f: setTime},
    command{name: "get-listener", f: getListener},
    command{name: "set-listener", f: setListener},
    command{name: "get-door-control", f: getDoorControl},
    command{name: "set-door-control", f: setDoorControl},
    command{name: "get-status", f: getStatus},
    command{name: "open-door", f: openDoor},
    command{name: "get-cards", f: getCards},
    command{name: "get-card", f: getCard},
    command{name: "get-card-by-index", f: getCardByIndex},
    command{name: "put-card", f: putCard},
    command{name: "delete-card", f: deleteCard},
    command{name: "delete-all-cards", f: deleteAllCards},
    command{name: "get-event", f: getEvent},
    command{name: "get-event-index", f: getEventIndex},
    command{name: "set-event-index", f: setEventIndex},
    command{name: "record-special-events", f: recordSpecialEvents},
    command{name: "get-time-profile", f: getTimeProfile},
    command{name: "set-time-profile", f: setTimeProfile},
    command{name: "delete-all-time-profiles", f: deleteAllTimeProfiles},
    command{name: "add-task", f: addTask},
    command{name: "refresh-tasklist", f: refreshTaskList},
    command{name: "clear-tasklist", f: clearTaskList},
    command{name: "listen", f: listen},
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
    controller := CONTROLLER

    return uhppote.GetController(controller)
}

func setIP() (any, error) {
    controller := CONTROLLER
    address := ADDRESS
    netmask := NETMASK
    gateway := GATEWAY

    if err := uhppote.SetIP(controller, address, netmask, gateway); err != nil {
        return nil, err
    }

    return nil, nil
}

func getTime() (any, error) {
    controller := CONTROLLER

    return uhppote.GetTime(controller)
}

func setTime() (any, error) {
    controller := CONTROLLER
    now := uhppote.DateTime(time.Now())

    return uhppote.SetTime(controller, now)
}

func getStatus() (any, error) {
    controller := CONTROLLER

    return uhppote.GetStatus(controller)
}

func getListener() (any, error) {
    controller := CONTROLLER

    return uhppote.GetListener(controller)
}

func setListener() (any, error) {
    controller := CONTROLLER
    address := LISTENER.Addr()
    port := LISTENER.Port()

    return uhppote.SetListener(controller, address, port)
}

func getDoorControl() (any, error) {
    controller := CONTROLLER
    door := DOOR

    return uhppote.GetDoorControl(controller, door)
}

func setDoorControl() (any, error) {
    controller := CONTROLLER
    door := DOOR
    mode := MODE
    delay := DELAY

    return uhppote.SetDoorControl(controller, door, mode, delay)
}

func openDoor() (any, error) {
    controller := CONTROLLER
    door := DOOR

    return uhppote.OpenDoor(controller, door)
}

func getCards() (any, error) {
    controller := CONTROLLER

    return uhppote.GetCards(controller)
}

func getCard() (any, error) {
    controller := CONTROLLER
    card := CARD

    return uhppote.GetCard(controller, card)
}

func getCardByIndex() (any, error) {
    controller := CONTROLLER
    index := CARD_INDEX

    return uhppote.GetCardByIndex(controller, index)
}

func putCard() (any, error) {
    controller := CONTROLLER
    card := CARD
    start, _ := time.Parse("2006-01-02", "2022-01-01")
    end, _ := time.Parse("2006-01-02", "2022-12-31")

    return uhppote.PutCard(controller, card, uhppote.Date(start), uhppote.Date(end), 0, 1, 29, 0)
}

func deleteCard() (any, error) {
    controller := CONTROLLER
    card := CARD

    return uhppote.DeleteCard(controller, card)
}

func deleteAllCards() (any, error) {
    controller := CONTROLLER

    return uhppote.DeleteAllCards(controller)
}

func getEvent() (any, error) {
    controller := CONTROLLER
    index := EVENT_INDEX

    return uhppote.GetEvent(controller, index)
}

func getEventIndex() (any, error) {
    controller := CONTROLLER

    return uhppote.GetEventIndex(controller)
}

func setEventIndex() (any, error) {
    controller := CONTROLLER
    index := EVENT_INDEX

    return uhppote.SetEventIndex(controller, index)
}

func recordSpecialEvents() (any, error) {
    controller := CONTROLLER
    enabled := true

    return uhppote.RecordSpecialEvents(controller, enabled)
}

func getTimeProfile() (any, error) {
    controller := CONTROLLER
    profileID := TIME_PROFILE_ID

    return uhppote.GetTimeProfile(controller, profileID)
}

func setTimeProfile() (any, error) {
    controller := CONTROLLER
    profileID := TIME_PROFILE_ID
    start, _ := time.Parse("2006-01-02", "2022-01-01")
    end, _ := time.Parse("2006-01-02", "2022-12-31")
    monday := true
    tuesday := true
    wednesday := false
    thursday := true
    friday := false
    saturday := false
    sunday := true
    segment1start, _ := time.Parse("15:04", "08:30")
    segment1end, _ := time.Parse("15:04", "11:45")
    segment2start, _ := time.Parse("15:04", "13:15")
    segment2end, _ := time.Parse("15:04", "16:30")
    segment3start, _ := time.Parse("15:04", "19:30")
    segment3end, _ := time.Parse("15:04", "20:55")
    linkedProfileID := uint8(30)

    return uhppote.SetTimeProfile(
        controller,
        profileID,
        uhppote.Date(start), uhppote.Date(end),
        monday, tuesday, wednesday, thursday, friday, saturday, sunday,
        uhppote.HHmm(segment1start), uhppote.HHmm(segment1end),
        uhppote.HHmm(segment2start), uhppote.HHmm(segment2end),
        uhppote.HHmm(segment3start), uhppote.HHmm(segment3end),
        linkedProfileID)
}

func deleteAllTimeProfiles() (any, error) {
    controller := CONTROLLER

    return uhppote.DeleteAllTimeProfiles(controller)
}

func addTask() (any, error) {
    controller := CONTROLLER
    startDate, _ := time.Parse("2006-01-02", "2022-01-01")
    endDate, _ := time.Parse("2006-01-02", "2022-12-31")
    monday := true
    tuesday := false
    wednesday := true
    thursday := true
    friday := false
    saturday := false
    sunday := true
    startTime, _ := time.Parse("15:04", "08:30")
    door := DOOR
    taskType := uint8(2)
    moreCards := uint8(0)

    return uhppote.AddTask(controller,
        uhppote.Date(startDate), uhppote.Date(endDate),
        monday, tuesday, wednesday, thursday, friday, saturday, sunday,
        uhppote.HHmm(startTime),
        door,
        taskType,
        moreCards)
}

func refreshTaskList() (any, error) {
    controller := CONTROLLER

    return uhppote.RefreshTasklist(controller)
}

func clearTaskList() (any, error) {
    controller := CONTROLLER

    return uhppote.ClearTasklist(controller)
}

func listen() (any, error) {
    events := make(chan uhppote.Event)
    errors := make(chan error)
    interrupt := make(chan os.Signal, 1)

    defer close(events)
    defer close(errors)
    defer close(interrupt)

    signal.Notify(interrupt, syscall.SIGINT, syscall.SIGTERM)

    go func() {
        for evt := range events {
            log.Printf("INFO  %+v", pprint(evt))
        }
    }()

    go func() {
        for err := range errors {
            log.Fatalf("ERROR  %v", err)
        }
    }()

    if err := uhppote.Listen(events, errors, interrupt); err != nil {
        return nil, err
    }

    return struct{}{}, nil
}
