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
}

var GetControllerResponse = Message{
	Name:    "get controller response",
	MsgType: 0x94,
	Fields: []Field{
		Field{
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "ip address",
			Type:   "IPv4",
			Offset: 8,
		},
		Field{
			Name:   "subnet mask",
			Type:   "IPv4",
			Offset: 12,
		},
		Field{
			Name:   "gateway",
			Type:   "IPv4",
			Offset: 16,
		},
		Field{
			Name:   "MAC address",
			Type:   "MAC",
			Offset: 20,
		},
		Field{
			Name:   "version",
			Type:   "version",
			Offset: 26,
		},
		Field{
			Name:   "date",
			Type:   "date",
			Offset: 28,
		},
	},
}

var GetTimeResponse = Message{
	Name:    "get time response",
	MsgType: 0x32,
	Fields: []Field{
		Field{
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "datetime",
			Type:   "datetime",
			Offset: 8,
		},
	},
}

var SetTimeResponse = Message{
	Name:    "set time response",
	MsgType: 0x30,
	Fields: []Field{
		Field{
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "datetime",
			Type:   "datetime",
			Offset: 8,
		},
	},
}

var GetStatusResponse = Message{
	Name:    "get status response",
	MsgType: 0x20,
	Fields: []Field{
		Field{
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "system date",
			Type:   "shortdate",
			Offset: 51,
		},
		Field{
			Name:   "system time",
			Type:   "time",
			Offset: 37,
		},
		Field{
			Name:   "door 1 open",
			Type:   "bool",
			Offset: 28,
		},
		Field{
			Name:   "door 2 open",
			Type:   "bool",
			Offset: 29,
		},
		Field{
			Name:   "door 3 open",
			Type:   "bool",
			Offset: 30,
		},
		Field{
			Name:   "door 4 open",
			Type:   "bool",
			Offset: 31,
		},
		Field{
			Name:   "door 1 button",
			Type:   "bool",
			Offset: 32,
		},
		Field{
			Name:   "door 2 button",
			Type:   "bool",
			Offset: 33,
		},
		Field{
			Name:   "door 3 button",
			Type:   "bool",
			Offset: 34,
		},
		Field{
			Name:   "door 4 button",
			Type:   "bool",
			Offset: 35,
		},
		Field{
			Name:   "relays",
			Type:   "uint8",
			Offset: 49,
		},
		Field{
			Name:   "inputs",
			Type:   "uint8",
			Offset: 50,
		},
		Field{
			Name:   "system error",
			Type:   "uint8",
			Offset: 36,
		},
		Field{
			Name:   "special info",
			Type:   "uint8",
			Offset: 48,
		},
		Field{
			Name:   "event index",
			Type:   "uint32",
			Offset: 8,
		},
		Field{
			Name:   "event type",
			Type:   "uint8",
			Offset: 12,
		},
		Field{
			Name:   "event access granted",
			Type:   "bool",
			Offset: 13,
		},
		Field{
			Name:   "event door",
			Type:   "uint8",
			Offset: 14,
		},
		Field{
			Name:   "event direction",
			Type:   "uint8",
			Offset: 15,
		},
		Field{
			Name:   "event card",
			Type:   "uint32",
			Offset: 16,
		},
		Field{
			Name:   "event timestamp",
			Type:   "optional datetime",
			Offset: 20,
		},
		Field{
			Name:   "event reason",
			Type:   "uint8",
			Offset: 27,
		},
		Field{
			Name:   "sequence no",
			Type:   "uint32",
			Offset: 40,
		},
	},
}

var GetListenerResponse = Message{
	Name:    "get listener response",
	MsgType: 0x92,
	Fields: []Field{
		Field{
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "address",
			Type:   "IPv4",
			Offset: 8,
		},
		Field{
			Name:   "port",
			Type:   "uint16",
			Offset: 12,
		},
	},
}

var SetListenerResponse = Message{
	Name:    "set listener response",
	MsgType: 0x90,
	Fields: []Field{
		Field{
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetDoorControlResponse = Message{
	Name:    "get door control response",
	MsgType: 0x82,
	Fields: []Field{
		Field{
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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

var SetDoorControlResponse = Message{
	Name:    "set door control response",
	MsgType: 0x80,
	Fields: []Field{
		Field{
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
	},
}

var GetCardByIndexResponse = Message{
	Name:    "get card by index response",
	MsgType: 0x5c,
	Fields: []Field{
		Field{
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
	},
}

var PutCardResponse = Message{
	Name:    "put card response",
	MsgType: 0x50,
	Fields: []Field{
		Field{
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
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
			Name:   "controller",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "cleared",
			Type:   "bool",
			Offset: 8,
		},
	},
}
