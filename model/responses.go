package model

import (
	"github.com/uhppoted/uhppoted-codegen/model/responses"
	"github.com/uhppoted/uhppoted-codegen/model/types"
)

var Responses = []types.Message{
	GetControllerResponse.Message,
	GetTimeResponse.Message,
	SetTimeResponse,
	GetListenerResponse.Message,
	SetListenerResponse,
	GetDoorControlResponse,
	SetDoorControlResponse,
	OpenDoorResponse,
	GetStatusResponse,
	GetCardsResponse.Message,
	GetCardResponse.Message,
	GetCardByIndexResponse.Message,
	PutCardResponse,
	DeleteCardResponse,
	DeleteAllCardsResponse,
	GetEventResponse,
	GetEventIndexResponse,
	SetEventIndexResponse,
	RecordSpecialEventsResponse,
	GetTimeProfileResponse,
	SetTimeProfileResponse,
	DeleteAllTimeProfilesResponse,
	AddTaskResponse,
	RefreshTaskListResponse,
	ClearTaskListResponse,
	SetPCControlResponse,
	SetInterlockResponse,
	ActivateKeypadsResponse,
	SetDoorPasscodesResponse,
	GetAntiPassbackResponse,
	SetAntiPassbackResponse,
	RestoreDefaultParametersResponse,
}

var GetControllerResponse = responses.GetControllerResponse
var GetTimeResponse = responses.GetTimeResponse
var GetListenerResponse = responses.GetListenerResponse
var GetCardsResponse = responses.GetCardsResponse
var GetCardResponse = responses.GetCardResponse
var GetCardByIndexResponse = responses.GetCardByIndexResponse

var SetTimeResponse = types.Message{
	Name:    "set time response",
	MsgType: 0x30,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:        "datetime",
			Type:        "datetime",
			Offset:      8,
			Description: "controller system date/time",
		},
	},
}

var GetStatusResponse = types.Message{
	Name:    "get status response",
	MsgType: 0x20,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:        "system date",
			Type:        "shortdate",
			Offset:      51,
			Description: "controller system date, e.g. 2025-07-21",
		},
		types.Field{
			Name:        "system time",
			Type:        "time",
			Offset:      37,
			Description: "controller system time, e.g. 13:25:47",
		},
		types.Field{
			Name:        "door 1 open",
			Type:        "bool",
			Offset:      28,
			Description: "door 1 open sensor",
		},
		types.Field{
			Name:        "door 2 open",
			Type:        "bool",
			Offset:      29,
			Description: "door 2 open sensor",
		},
		types.Field{
			Name:        "door 3 open",
			Type:        "bool",
			Offset:      30,
			Description: "door 3 open sensor",
		},
		types.Field{
			Name:        "door 4 open",
			Type:        "bool",
			Offset:      31,
			Description: "door 4 open sensor",
		},
		types.Field{
			Name:        "door 1 button",
			Type:        "bool",
			Offset:      32,
			Description: "door 1 button pressed",
		},
		types.Field{
			Name:        "door 2 button",
			Type:        "bool",
			Offset:      33,
			Description: "door 2 button pressed",
		},
		types.Field{
			Name:        "door 3 button",
			Type:        "bool",
			Offset:      34,
			Description: "door 3 button pressed",
		},
		types.Field{
			Name:        "door 4 button",
			Type:        "bool",
			Offset:      35,
			Description: "door 4 button pressed",
		},
		types.Field{
			Name:        "relays",
			Type:        "uint8",
			Offset:      49,
			Description: "bitset of door unlock relay states",
		},
		types.Field{
			Name:        "inputs",
			Type:        "uint8",
			Offset:      50,
			Description: "bitset of alarm inputs",
		},
		types.Field{
			Name:        "system error",
			Type:        "uint8",
			Offset:      36,
			Description: "system error code",
		},
		types.Field{
			Name:        "special info",
			Type:        "uint8",
			Offset:      48,
			Description: "absolutely no idea",
		},
		types.Field{
			Name:        "event index",
			Type:        "uint32",
			Offset:      8,
			Description: "last event index",
		},
		types.Field{
			Name:        "event type",
			Type:        "uint8",
			Offset:      12,
			Description: "last event type",
		},
		types.Field{
			Name:        "event access granted",
			Type:        "bool",
			Offset:      13,
			Description: "last event access granted",
		},
		types.Field{
			Name:        "event door",
			Type:        "uint8",
			Offset:      14,
			Description: "last event door",
		},
		types.Field{
			Name:        "event direction",
			Type:        "uint8",
			Offset:      15,
			Description: "last event door direction (0: in, 1: out)",
		},
		types.Field{
			Name:        "event card",
			Type:        "uint32",
			Offset:      16,
			Description: "last event card number",
		},
		types.Field{
			Name:        "event timestamp",
			Type:        "optional datetime",
			Offset:      20,
			Description: "last event timestamp",
		},
		types.Field{
			Name:        "event reason",
			Type:        "uint8",
			Offset:      27,
			Description: "last event reason",
		},
		types.Field{
			Name:        "sequence no",
			Type:        "uint32",
			Offset:      40,
			Description: "packet sequence number",
		},
	},
}

// var GetListenerResponse = types.Message{
// 	Name:    "get listener response",
// 	MsgType: 0x92,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:        "address",
// 			Type:        "IPv4",
// 			Offset:      8,
// 			Description: "event listener IPv4 address",
// 		},
// 		types.Field{
// 			Name:        "port",
// 			Type:        "uint16",
// 			Offset:      12,
// 			Description: "event listener IPv4 port",
// 		},
// 		types.Field{
// 			Name:        "interval",
// 			Type:        "uint8",
// 			Offset:      14,
// 			Description: "status auto-send interval (seconds)",
// 		},
// 	},
// }

var SetListenerResponse = types.Message{
	Name:    "set listener response",
	MsgType: 0x90,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:        "ok",
			Type:        "bool",
			Offset:      8,
			Description: "set-listener succeeded/failed",
		},
	},
}

var GetDoorControlResponse = types.Message{
	Name:    "get door control response",
	MsgType: 0x82,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:        "door",
			Type:        "uint8",
			Offset:      8,
			Description: "door ID ([1..4]",
		},
		types.Field{
			Name:        "mode",
			Type:        "uint8",
			Offset:      9,
			Description: "control mode (1:normally open, 2:normally closed. 3:controlled)",
		},
		types.Field{
			Name:        "delay",
			Type:        "uint8",
			Offset:      10,
			Description: "unlock delay (seconds)",
		},
	},
}

var SetDoorControlResponse = types.Message{
	Name:    "set door control response",
	MsgType: 0x80,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "door",
			Type:   "uint8",
			Offset: 8,
		},
		types.Field{
			Name:   "mode",
			Type:   "uint8",
			Offset: 9,
		},
		types.Field{
			Name:   "delay",
			Type:   "uint8",
			Offset: 10,
		},
	},
}

var OpenDoorResponse = types.Message{
	Name:    "open door response",
	MsgType: 0x40,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "opened",
			Type:   "bool",
			Offset: 8,
		},
	},
}

// var GetCardByIndexResponse = types.Message{
// 	Name:    "get card by index response",
// 	MsgType: 0x5c,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "card number",
// 			Type:   "uint32",
// 			Offset: 8,
// 		},
// 		types.Field{
// 			Name:   "start date",
// 			Type:   "optional date",
// 			Offset: 12,
// 		},
// 		types.Field{
// 			Name:   "end date",
// 			Type:   "optional date",
// 			Offset: 16,
// 		},
// 		types.Field{
// 			Name:   "door 1",
// 			Type:   "uint8",
// 			Offset: 20,
// 		},
// 		types.Field{
// 			Name:   "door 2",
// 			Type:   "uint8",
// 			Offset: 21,
// 		},
// 		types.Field{
// 			Name:   "door 3",
// 			Type:   "uint8",
// 			Offset: 22,
// 		},
// 		types.Field{
// 			Name:   "door 4",
// 			Type:   "uint8",
// 			Offset: 23,
// 		},
// 		types.Field{
// 			Name:   "PIN",
// 			Type:   "pin",
// 			Offset: 24,
// 		},
// 	},
// }

var PutCardResponse = types.Message{
	Name:    "put card response",
	MsgType: 0x50,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "stored",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var DeleteCardResponse = types.Message{
	Name:    "delete card response",
	MsgType: 0x52,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "deleted",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var DeleteAllCardsResponse = types.Message{
	Name:    "delete all cards response",
	MsgType: 0x54,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "deleted",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetEventResponse = types.Message{
	Name:    "get event response",
	MsgType: 0xb0,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "index",
			Type:   "uint32",
			Offset: 8,
		},
		types.Field{
			Name:   "event type",
			Type:   "uint8",
			Offset: 12,
		},
		types.Field{
			Name:   "access granted",
			Type:   "bool",
			Offset: 13,
		},
		types.Field{
			Name:   "door",
			Type:   "uint8",
			Offset: 14,
		},
		types.Field{
			Name:   "direction",
			Type:   "uint8",
			Offset: 15,
		},
		types.Field{
			Name:   "card",
			Type:   "uint32",
			Offset: 16,
		},
		types.Field{
			Name:   "timestamp",
			Type:   "optional datetime",
			Offset: 20,
		},
		types.Field{
			Name:   "reason",
			Type:   "uint8",
			Offset: 27,
		},
	},
}

var GetEventIndexResponse = types.Message{
	Name:    "get event index response",
	MsgType: 0xb4,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "event index",
			Type:   "uint32",
			Offset: 8,
		},
	},
}

var SetEventIndexResponse = types.Message{
	Name:    "set event index response",
	MsgType: 0xb2,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "updated",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var RecordSpecialEventsResponse = types.Message{
	Name:    "record special events response",
	MsgType: 0x8e,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "updated",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetTimeProfileResponse = types.Message{
	Name:    "get time profile response",
	MsgType: 0x98,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "profile id",
			Type:   "uint8",
			Offset: 8,
		},
		types.Field{
			Name:   "start date",
			Type:   "optional date",
			Offset: 9,
		},
		types.Field{
			Name:   "end date",
			Type:   "optional date",
			Offset: 13,
		},
		types.Field{
			Name:   "monday",
			Type:   "bool",
			Offset: 17,
		},
		types.Field{
			Name:   "tuesday",
			Type:   "bool",
			Offset: 18,
		},
		types.Field{
			Name:   "wednesday",
			Type:   "bool",
			Offset: 19,
		},
		types.Field{
			Name:   "thursday",
			Type:   "bool",
			Offset: 20,
		},
		types.Field{
			Name:   "friday",
			Type:   "bool",
			Offset: 21,
		},
		types.Field{
			Name:   "saturday",
			Type:   "bool",
			Offset: 22,
		},
		types.Field{
			Name:   "sunday",
			Type:   "bool",
			Offset: 23,
		},
		types.Field{
			Name:   "segment 1 start",
			Type:   "HHmm",
			Offset: 24,
		},
		types.Field{
			Name:   "segment 1 end",
			Type:   "HHmm",
			Offset: 26,
		},
		types.Field{
			Name:   "segment 2 start",
			Type:   "HHmm",
			Offset: 28,
		},
		types.Field{
			Name:   "segment 2 end",
			Type:   "HHmm",
			Offset: 30,
		},
		types.Field{
			Name:   "segment 3 start",
			Type:   "HHmm",
			Offset: 32,
		},
		types.Field{
			Name:   "segment 3 end",
			Type:   "HHmm",
			Offset: 34,
		},
		types.Field{
			Name:   "linked profile id",
			Type:   "uint8",
			Offset: 36,
		},
	},
}

var SetTimeProfileResponse = types.Message{
	Name:    "set time profile response",
	MsgType: 0x88,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "stored",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var DeleteAllTimeProfilesResponse = types.Message{
	Name:    "delete all time profiles response",
	MsgType: 0x8a,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "deleted",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var AddTaskResponse = types.Message{
	Name:    "add task response",
	MsgType: 0xa8,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "added",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var RefreshTaskListResponse = types.Message{
	Name:    "refresh tasklist response",
	MsgType: 0xac,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "refreshed",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var ClearTaskListResponse = types.Message{
	Name:    "clear tasklist response",
	MsgType: 0xa6,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "cleared",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var SetPCControlResponse = types.Message{
	Name:    "set pc control response",
	MsgType: 0xa0,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var SetInterlockResponse = types.Message{
	Name:    "set interlock response",
	MsgType: 0xa2,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var ActivateKeypadsResponse = types.Message{
	Name:    "activate keypads response",
	MsgType: 0xa4,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var SetDoorPasscodesResponse = types.Message{
	Name:    "set door passcodes response",
	MsgType: 0x8c,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetAntiPassbackResponse = types.Message{
	Name:    "get antipassback response",
	MsgType: 0x86,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "antipassback",
			Type:   "uint8",
			Offset: 8,
		},
	},
}

var SetAntiPassbackResponse = types.Message{
	Name:    "set antipassback response",
	MsgType: 0x84,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var RestoreDefaultParametersResponse = types.Message{
	Name:    "restore default parameters response",
	MsgType: 0xc8,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "reset",
			Type:   "bool",
			Offset: 8,
		},
	},
}
