import * as uhppote from './uhppote.js'

export const commands = new Map([
  ['get-all-controllers', { fn: getAllControllers, args: [] }],
  ['get-controller', { fn: getController, args: ['device-id'] }],
  ['set-IP', { fn: setIP, args: ['device-id', 'ip-address', 'subnet', 'gateway'] }],
  ['get-time', { fn: getTime, args: ['device-id'] }],
  ['set-time', { fn: setTime, args: ['device-id', 'datetime'] }]
])

export function exec (cmd) {
  return cmd.fn()
}

function getAllControllers () {
  return uhppote.GetAllControllers()
}

function getController () {
  const controller = document.querySelector('input#device-id').value

  return uhppote.GetController(controller)
}

function setIP () {
  const controller = document.querySelector('input#device-id').value
  const address = document.querySelector('input#ip-address').value
  const netmask = document.querySelector('input#subnet').value
  const gateway = document.querySelector('input#gateway').value

  return uhppote.SetIP(controller, address, netmask, gateway)
}

function getTime () {
  const controller = document.querySelector('input#device-id').value

  return uhppote.GetTime(controller)
}

function setTime () {
  const controller = document.querySelector('input#device-id').value
  const datetime = document.querySelector('input#datetime').value

  if (datetime === '') {
    return uhppote.SetTime(controller, new Date())
  } else {
    return uhppote.SetTime(controller, new Date(datetime))
  }
}

function getStatus() (any, error) {
  const controller = document.querySelector('input#device-id').value

  return uhppote.GetStatus(controller)
}

function getListener() (any, error) {
  const controller = document.querySelector('input#device-id').value

  return uhppote.GetListener(controller)
}

// function setListener() (any, error) {
//   const controller = document.querySelector('input#device-id').value
//   const listener = document.querySelector('input#listener').value
// 
//   address := LISTENER.Addr()
//   port := LISTENER.Port()
// 
//   return uhppote.SetListener(controller, address, port)
// }

function getDoorControl() (any, error) {
  const controller = document.querySelector('input#device-id').value
  const door = document.querySelector('input#door').value

  return uhppote.GetDoorControl(controller, door)
}

function setDoorControl() (any, error) {
  const controller = document.querySelector('input#device-id').value
  const door = document.querySelector('input#door').value
  const mode = document.querySelector('input#mode').value
  const delay = document.querySelector('input#delay').value

  return uhppote.SetDoorControl(controller, door, mode, delay)
}

function openDoor() (any, error) {
  const controller = document.querySelector('input#device-id').value
  const door = document.querySelector('input#door').value

  return uhppote.OpenDoor(controller, door)
}

function getCards() (any, error) {
  const controller = document.querySelector('input#device-id').value

  return uhppote.GetCards(controller)
}

function getCard() (any, error) {
  const controller = document.querySelector('input#device-id').value
  const card = document.querySelector('input#card').value

  return uhppote.GetCard(controller, card)
}

function getCardByIndex() (any, error) {
  const controller = document.querySelector('input#device-id').value
  const index = document.querySelector('input#card-index').value

  return uhppote.GetCardByIndex(controller, index)
}

// func putCard() (any, error) {
//     controller := CONTROLLER
//     card := CARD
//     start, _ := time.Parse("2006-01-02", "2022-01-01")
//     end, _ := time.Parse("2006-01-02", "2022-12-31")

//     return uhppote.PutCard(controller, card, uhppote.Date(start), uhppote.Date(end), 0, 1, 29, 0)
// }

function deleteCard() (any, error) {
  const controller = document.querySelector('input#device-id').value
  const card = document.querySelector('input#card').value

  return uhppote.DeleteCard(controller, card)
}

function deleteAllCards() (any, error) {
  const controller = document.querySelector('input#device-id').value

  return uhppote.DeleteAllCards(controller)
}

function getEvent() (any, error) {
  const controller = document.querySelector('input#device-id').value
  const index = document.querySelector('input#event-index').value

  return uhppote.GetEvent(controller, index)
}

function getEventIndex() (any, error) {
  const controller = document.querySelector('input#device-id').value

  return uhppote.GetEventIndex(controller)
}

function setEventIndex() (any, error) {
  const controller = document.querySelector('input#device-id').value
  const index = document.querySelector('input#event-index').value

  return uhppote.SetEventIndex(controller, index)
}

function recordSpecialEvents() (any, error) {
  const controller = document.querySelector('input#device-id').value
  const enabled = document.querySelector('input#record-special-events').checked

  return uhppote.RecordSpecialEvents(controller, enabled)
}

function getTimeProfile() (any, error) {
  const controller = document.querySelector('input#device-id').value
  const profileID = document.querySelector('input#time-profile').value

  return uhppote.GetTimeProfile(controller, profileID)
}

// func setTimeProfile() (any, error) {
//     controller := CONTROLLER
//     profileID := TIME_PROFILE_ID
//     start, _ := time.Parse("2006-01-02", "2022-01-01")
//     end, _ := time.Parse("2006-01-02", "2022-12-31")
//     monday := true
//     tuesday := true
//     wednesday := false
//     thursday := true
//     friday := false
//     saturday := false
//     sunday := true
//     segment1start, _ := time.Parse("15:04", "08:30")
//     segment1end, _ := time.Parse("15:04", "11:45")
//     segment2start, _ := time.Parse("15:04", "13:15")
//     segment2end, _ := time.Parse("15:04", "16:30")
//     segment3start, _ := time.Parse("15:04", "19:30")
//     segment3end, _ := time.Parse("15:04", "20:55")
//     linkedProfileID := uint8(30)

//     return uhppote.SetTimeProfile(
//         controller,
//         profileID,
//         uhppote.Date(start), uhppote.Date(end),
//         monday, tuesday, wednesday, thursday, friday, saturday, sunday,
//         uhppote.HHmm(segment1start), uhppote.HHmm(segment1end),
//         uhppote.HHmm(segment2start), uhppote.HHmm(segment2end),
//         uhppote.HHmm(segment3start), uhppote.HHmm(segment3end),
//         linkedProfileID)
// }

function deleteAllTimeProfiles() (any, error) {
  const controller = document.querySelector('input#device-id').value

  return uhppote.DeleteAllTimeProfiles(controller)
}

// func addTask() (any, error) {
//     controller := CONTROLLER
//     startDate, _ := time.Parse("2006-01-02", "2022-01-01")
//     endDate, _ := time.Parse("2006-01-02", "2022-12-31")
//     monday := true
//     tuesday := false
//     wednesday := true
//     thursday := true
//     friday := false
//     saturday := false
//     sunday := true
//     startTime, _ := time.Parse("15:04", "08:30")
//     door := DOOR
//     taskType := uint8(2)
//     moreCards := uint8(0)

//     return uhppote.AddTask(controller,
//         uhppote.Date(startDate), uhppote.Date(endDate),
//         monday, tuesday, wednesday, thursday, friday, saturday, sunday,
//         uhppote.HHmm(startTime),
//         door,
//         taskType,
//         moreCards)
// }

function refreshTaskList() (any, error) {
  const controller = document.querySelector('input#device-id').value

  return uhppote.RefreshTasklist(controller)
}

function clearTaskList() (any, error) {
  const controller = document.querySelector('input#device-id').value

  return uhppote.ClearTasklist(controller)
}

