package model

var Responses = []Message{
	GetControllerResponse,
	GetTimeResponse,
	SetTimeResponse,
	GetStatusResponse,
	GetListenerResponse,
	SetListenerResponse,
	GetDoorControlResponse,
	SetDoorControlResponse,
	OpenDoorResponse,
	GetCardsResponse,
	GetCardResponse,
	GetCardByIndexResponse,
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

var GetControllerResponse = Message{
	Name:    "get controller response",
	MsgType: 0x94,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:        "ip address",
			Type:        "IPv4",
			Offset:      8,
			Description: "controller IPv4 address, e.g. 192.168.1.100",
		},
		Field{
			Name:        "subnet mask",
			Type:        "IPv4",
			Offset:      12,
			Description: "controller IPv4 netmask, e.g. 255.255.255.0",
		},
		Field{
			Name:        "gateway",
			Type:        "IPv4",
			Offset:      16,
			Description: "controller IPv4 gateway address, e.g. 192.168.1.1",
		},
		Field{
			Name:        "MAC address",
			Type:        "MAC",
			Offset:      20,
			Description: "controller MAC address, e.g. 52:fd:fc:07:21:82",
		},
		Field{
			Name:        "version",
			Type:        "version",
			Offset:      26,
			Description: "controller firmware version, e.g. v6.62",
		},
		Field{
			Name:        "date",
			Type:        "date",
			Offset:      28,
			Description: "controller firmware release date, e.g. 2020-12-31",
		},
	},
}

var GetTimeResponse = Message{
	Name:    "get time response",
	MsgType: 0x32,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:        "datetime",
			Type:        "datetime",
			Offset:      8,
			Description: "controller system date/time",
		},
	},
}

var SetTimeResponse = Message{
	Name:    "set time response",
	MsgType: 0x30,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:        "datetime",
			Type:        "datetime",
			Offset:      8,
			Description: "controller system date/time",
		},
	},
}

var GetStatusResponse = Message{
	Name:    "get status response",
	MsgType: 0x20,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:        "system date",
			Type:        "shortdate",
			Offset:      51,
			Description: "controller system date, e.g. 2025-07-21",
		},
		Field{
			Name:        "system time",
			Type:        "time",
			Offset:      37,
			Description: "controller system time, e.g. 13:25:47",
		},
		Field{
			Name:        "door 1 open",
			Type:        "bool",
			Offset:      28,
			Description: "door 1 open sensor",
		},
		Field{
			Name:        "door 2 open",
			Type:        "bool",
			Offset:      29,
			Description: "door 2 open sensor",
		},
		Field{
			Name:        "door 3 open",
			Type:        "bool",
			Offset:      30,
			Description: "door 3 open sensor",
		},
		Field{
			Name:        "door 4 open",
			Type:        "bool",
			Offset:      31,
			Description: "door 4 open sensor",
		},
		Field{
			Name:        "door 1 button",
			Type:        "bool",
			Offset:      32,
			Description: "door 1 button pressed",
		},
		Field{
			Name:        "door 2 button",
			Type:        "bool",
			Offset:      33,
			Description: "door 2 button pressed",
		},
		Field{
			Name:        "door 3 button",
			Type:        "bool",
			Offset:      34,
			Description: "door 3 button pressed",
		},
		Field{
			Name:        "door 4 button",
			Type:        "bool",
			Offset:      35,
			Description: "door 4 button pressed",
		},
		Field{
			Name:        "relays",
			Type:        "uint8",
			Offset:      49,
			Description: "bitset of door unlock relay states",
		},
		Field{
			Name:        "inputs",
			Type:        "uint8",
			Offset:      50,
			Description: "bitset of alarm inputs",
		},
		Field{
			Name:        "system error",
			Type:        "uint8",
			Offset:      36,
			Description: "system error code",
		},
		Field{
			Name:        "special info",
			Type:        "uint8",
			Offset:      48,
			Description: "absolutely no idea",
		},
		Field{
			Name:        "event index",
			Type:        "uint32",
			Offset:      8,
			Description: "last event index",
		},
		Field{
			Name:        "event type",
			Type:        "uint8",
			Offset:      12,
			Description: "last event type",
		},
		Field{
			Name:        "event access granted",
			Type:        "bool",
			Offset:      13,
			Description: "last event access granted",
		},
		Field{
			Name:        "event door",
			Type:        "uint8",
			Offset:      14,
			Description: "last event door",
		},
		Field{
			Name:        "event direction",
			Type:        "uint8",
			Offset:      15,
			Description: "last event door direction (0: in, 1: out)",
		},
		Field{
			Name:        "event card",
			Type:        "uint32",
			Offset:      16,
			Description: "last event card number",
		},
		Field{
			Name:        "event timestamp",
			Type:        "optional datetime",
			Offset:      20,
			Description: "last event timestamp",
		},
		Field{
			Name:        "event reason",
			Type:        "uint8",
			Offset:      27,
			Description: "last event reason",
		},
		Field{
			Name:        "sequence no",
			Type:        "uint32",
			Offset:      40,
			Description: "packet sequence number",
		},
	},
}

var GetListenerResponse = Message{
	Name:    "get listener response",
	MsgType: 0x92,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:        "address",
			Type:        "IPv4",
			Offset:      8,
			Description: "event listener IPv4 address",
		},
		Field{
			Name:        "port",
			Type:        "uint16",
			Offset:      12,
			Description: "event listener IPv4 port",
		},
		Field{
			Name:        "interval",
			Type:        "uint8",
			Offset:      14,
			Description: "status auto-send interval (seconds)",
		},
	},
}

var SetListenerResponse = Message{
	Name:    "set listener response",
	MsgType: 0x90,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:        "ok",
			Type:        "bool",
			Offset:      8,
			Description: "set-listener succeeded/failed",
		},
	},
}

var GetDoorControlResponse = Message{
	Name:    "get door control response",
	MsgType: 0x82,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:        "door",
			Type:        "uint8",
			Offset:      8,
			Description: "door ID ([1..4]",
		},
		Field{
			Name:        "mode",
			Type:        "uint8",
			Offset:      9,
			Description: "control mode (1:normally open, 2:normally closed. 3:controlled)",
		},
		Field{
			Name:        "delay",
			Type:        "uint8",
			Offset:      10,
			Description: "unlock delay (seconds)",
		},
	},
}

var SetDoorControlResponse = Message{
	Name:    "set door control response",
	MsgType: 0x80,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "door",
			Type:   "uint8",
			Offset: 8,
		},
		Field{
			Name:   "mode",
			Type:   "uint8",
			Offset: 9,
		},
		Field{
			Name:   "delay",
			Type:   "uint8",
			Offset: 10,
		},
	},
}

var OpenDoorResponse = Message{
	Name:    "open door response",
	MsgType: 0x40,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "opened",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetCardsResponse = Message{
	Name:    "get cards response",
	MsgType: 0x58,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "cards",
			Type:   "uint32",
			Offset: 8,
		},
	},
}

var GetCardResponse = Message{
	Name:    "get card response",
	MsgType: 0x5a,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "card number",
			Type:   "uint32",
			Offset: 8,
		},
		Field{
			Name:   "start date",
			Type:   "optional date",
			Offset: 12,
		},
		Field{
			Name:   "end date",
			Type:   "optional date",
			Offset: 16,
		},
		Field{
			Name:   "door 1",
			Type:   "uint8",
			Offset: 20,
		},
		Field{
			Name:   "door 2",
			Type:   "uint8",
			Offset: 21,
		},
		Field{
			Name:   "door 3",
			Type:   "uint8",
			Offset: 22,
		},
		Field{
			Name:   "door 4",
			Type:   "uint8",
			Offset: 23,
		},
		Field{
			Name:   "PIN",
			Type:   "pin",
			Offset: 24,
		},
	},
}

var GetCardByIndexResponse = Message{
	Name:    "get card by index response",
	MsgType: 0x5c,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "card number",
			Type:   "uint32",
			Offset: 8,
		},
		Field{
			Name:   "start date",
			Type:   "optional date",
			Offset: 12,
		},
		Field{
			Name:   "end date",
			Type:   "optional date",
			Offset: 16,
		},
		Field{
			Name:   "door 1",
			Type:   "uint8",
			Offset: 20,
		},
		Field{
			Name:   "door 2",
			Type:   "uint8",
			Offset: 21,
		},
		Field{
			Name:   "door 3",
			Type:   "uint8",
			Offset: 22,
		},
		Field{
			Name:   "door 4",
			Type:   "uint8",
			Offset: 23,
		},
		Field{
			Name:   "PIN",
			Type:   "pin",
			Offset: 24,
		},
	},
}

var PutCardResponse = Message{
	Name:    "put card response",
	MsgType: 0x50,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "stored",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var DeleteCardResponse = Message{
	Name:    "delete card response",
	MsgType: 0x52,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "deleted",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var DeleteAllCardsResponse = Message{
	Name:    "delete all cards response",
	MsgType: 0x54,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "deleted",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetEventResponse = Message{
	Name:    "get event response",
	MsgType: 0xb0,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "index",
			Type:   "uint32",
			Offset: 8,
		},
		Field{
			Name:   "event type",
			Type:   "uint8",
			Offset: 12,
		},
		Field{
			Name:   "access granted",
			Type:   "bool",
			Offset: 13,
		},
		Field{
			Name:   "door",
			Type:   "uint8",
			Offset: 14,
		},
		Field{
			Name:   "direction",
			Type:   "uint8",
			Offset: 15,
		},
		Field{
			Name:   "card",
			Type:   "uint32",
			Offset: 16,
		},
		Field{
			Name:   "timestamp",
			Type:   "optional datetime",
			Offset: 20,
		},
		Field{
			Name:   "reason",
			Type:   "uint8",
			Offset: 27,
		},
	},
}

var GetEventIndexResponse = Message{
	Name:    "get event index response",
	MsgType: 0xb4,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "event index",
			Type:   "uint32",
			Offset: 8,
		},
	},
}

var SetEventIndexResponse = Message{
	Name:    "set event index response",
	MsgType: 0xb2,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "updated",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var RecordSpecialEventsResponse = Message{
	Name:    "record special events response",
	MsgType: 0x8e,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "updated",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetTimeProfileResponse = Message{
	Name:    "get time profile response",
	MsgType: 0x98,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "profile id",
			Type:   "uint8",
			Offset: 8,
		},
		Field{
			Name:   "start date",
			Type:   "optional date",
			Offset: 9,
		},
		Field{
			Name:   "end date",
			Type:   "optional date",
			Offset: 13,
		},
		Field{
			Name:   "monday",
			Type:   "bool",
			Offset: 17,
		},
		Field{
			Name:   "tuesday",
			Type:   "bool",
			Offset: 18,
		},
		Field{
			Name:   "wednesday",
			Type:   "bool",
			Offset: 19,
		},
		Field{
			Name:   "thursday",
			Type:   "bool",
			Offset: 20,
		},
		Field{
			Name:   "friday",
			Type:   "bool",
			Offset: 21,
		},
		Field{
			Name:   "saturday",
			Type:   "bool",
			Offset: 22,
		},
		Field{
			Name:   "sunday",
			Type:   "bool",
			Offset: 23,
		},
		Field{
			Name:   "segment 1 start",
			Type:   "HHmm",
			Offset: 24,
		},
		Field{
			Name:   "segment 1 end",
			Type:   "HHmm",
			Offset: 26,
		},
		Field{
			Name:   "segment 2 start",
			Type:   "HHmm",
			Offset: 28,
		},
		Field{
			Name:   "segment 2 end",
			Type:   "HHmm",
			Offset: 30,
		},
		Field{
			Name:   "segment 3 start",
			Type:   "HHmm",
			Offset: 32,
		},
		Field{
			Name:   "segment 3 end",
			Type:   "HHmm",
			Offset: 34,
		},
		Field{
			Name:   "linked profile id",
			Type:   "uint8",
			Offset: 36,
		},
	},
}

var SetTimeProfileResponse = Message{
	Name:    "set time profile response",
	MsgType: 0x88,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "stored",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var DeleteAllTimeProfilesResponse = Message{
	Name:    "delete all time profiles response",
	MsgType: 0x8a,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "deleted",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var AddTaskResponse = Message{
	Name:    "add task response",
	MsgType: 0xa8,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "added",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var RefreshTaskListResponse = Message{
	Name:    "refresh tasklist response",
	MsgType: 0xac,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "refreshed",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var ClearTaskListResponse = Message{
	Name:    "clear tasklist response",
	MsgType: 0xa6,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "cleared",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var SetPCControlResponse = Message{
	Name:    "set pc control response",
	MsgType: 0xa0,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var SetInterlockResponse = Message{
	Name:    "set interlock response",
	MsgType: 0xa2,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var ActivateKeypadsResponse = Message{
	Name:    "activate keypads response",
	MsgType: 0xa4,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var SetDoorPasscodesResponse = Message{
	Name:    "set door passcodes response",
	MsgType: 0x8c,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetAntiPassbackResponse = Message{
	Name:    "get antipassback response",
	MsgType: 0x86,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "antipassback",
			Type:   "uint8",
			Offset: 8,
		},
	},
}

var SetAntiPassbackResponse = Message{
	Name:    "set antipassback response",
	MsgType: 0x84,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var RestoreDefaultParametersResponse = Message{
	Name:    "restore default parameters response",
	MsgType: 0xc8,
	Fields: []Field{
		Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		Field{
			Name:   "reset",
			Type:   "bool",
			Offset: 8,
		},
	},
}
