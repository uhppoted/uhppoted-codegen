package model

var Requests = []Message{
	GetControllerRequest,
	SetIPRequest,
	GetTimeRequest,
	SetTimeRequest,
	GetStatusRequest,
	GetListenerRequest,
	SetListenerRequest,
	GetDoorControlRequest,
	SetDoorControlRequest,
	OpenDoorRequest,
	GetCardsRequest,
	GetCardRequest,
	GetCardByIndexRequest,
	PutCardRequest,
	DeleteCardRequest,
	DeleteAllCardsRequest,
	GetEventRequest,
	GetEventIndexRequest,
	SetEventIndexRequest,
	RecordSpecialEventsRequest,
	GetTimeProfileRequest,
	SetTimeProfileRequest,
	DeleteAllTimeProfilesRequest,
	AddTaskRequest,
	RefreshTaskListRequest,
	ClearTaskListRequest,
	SetPCControlRequest,
}

var GetControllerRequest = Message{
	Name:    "get controller request",
	MsgType: 0x94,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
}

var SetIPRequest = Message{
	Name:    "set IP request",
	MsgType: 0x96,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "address",
			Type:   "IPv4",
			Offset: 8,
		},
		Field{
			Name:   "netmask",
			Type:   "IPv4",
			Offset: 12,
		},
		Field{
			Name:   "gateway",
			Type:   "IPv4",
			Offset: 16,
		},
		Field{
			Name:   "",
			Type:   "magic",
			Offset: 20,
		},
	},
}

var GetTimeRequest = Message{
	Name:    "get time request",
	MsgType: 0x32,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
}

var SetTimeRequest = Message{
	Name:    "set time request",
	MsgType: 0x30,
	Fields: []Field{
		Field{
			Name:   "device id",
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

var GetStatusRequest = Message{
	Name:    "get status request",
	MsgType: 0x20,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
}

var GetListenerRequest = Message{
	Name:    "get listener request",
	MsgType: 0x92,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
}

var SetListenerRequest = Message{
	Name:    "set listener request",
	MsgType: 0x90,
	Fields: []Field{
		Field{
			Name:   "device id",
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

var GetDoorControlRequest = Message{
	Name:    "get door control request",
	MsgType: 0x82,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "door",
			Type:   "uint8",
			Offset: 8,
		},
	},
}

var SetDoorControlRequest = Message{
	Name:    "set door control request",
	MsgType: 0x80,
	Fields: []Field{
		Field{
			Name:   "device id",
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

var OpenDoorRequest = Message{
	Name:    "open door request",
	MsgType: 0x40,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "door",
			Type:   "uint8",
			Offset: 8,
		},
	},
}

var GetCardsRequest = Message{
	Name:    "get cards request",
	MsgType: 0x58,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
}

var GetCardRequest = Message{
	Name:    "get card request",
	MsgType: 0x5a,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "card number",
			Type:   "uint32",
			Offset: 8,
		},
	},
}

var GetCardByIndexRequest = Message{
	Name:    "get card by index request",
	MsgType: 0x5c,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "card index",
			Type:   "uint32",
			Offset: 8,
		},
	},
}

var PutCardRequest = Message{
	Name:    "put card request",
	MsgType: 0x50,
	Fields: []Field{
		Field{
			Name:   "device id",
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
			Type:   "date",
			Offset: 12,
		},
		Field{
			Name:   "end date",
			Type:   "date",
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

var DeleteCardRequest = Message{
	Name:    "delete card request",
	MsgType: 0x52,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "card number",
			Type:   "uint32",
			Offset: 8,
		},
	},
}

var DeleteAllCardsRequest = Message{
	Name:    "delete cards request",
	MsgType: 0x54,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "",
			Type:   "magic",
			Offset: 8,
		},
	},
}

var GetEventRequest = Message{
	Name:    "get event request",
	MsgType: 0xb0,
	Fields: []Field{
		Field{
			Name:   "device id",
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

var GetEventIndexRequest = Message{
	Name:    "get event index request",
	MsgType: 0xb4,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
}

var SetEventIndexRequest = Message{
	Name:    "set event index request",
	MsgType: 0xb2,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "event index",
			Type:   "uint32",
			Offset: 8,
		},
		Field{
			Name:   "",
			Type:   "magic",
			Offset: 12,
		},
	},
}

var RecordSpecialEventsRequest = Message{
	Name:    "record special events request",
	MsgType: 0x8e,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "enable",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetTimeProfileRequest = Message{
	Name:    "get time profile request",
	MsgType: 0x98,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "profile id",
			Type:   "uint8",
			Offset: 8,
		},
	},
}

var SetTimeProfileRequest = Message{
	Name:    "set time profile request",
	MsgType: 0x88,
	Fields: []Field{
		Field{
			Name:   "device id",
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
			Type:   "date",
			Offset: 9,
		},
		Field{
			Name:   "end date",
			Type:   "date",
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

var DeleteAllTimeProfilesRequest = Message{
	Name:    "delete all time profiles request",
	MsgType: 0x8a,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "",
			Type:   "magic",
			Offset: 8,
		},
	},
}

var AddTaskRequest = Message{
	Name:    "add task request",
	MsgType: 0xa8,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "start date",
			Type:   "date",
			Offset: 8,
		},
		Field{
			Name:   "end date",
			Type:   "date",
			Offset: 12,
		},
		Field{
			Name:   "monday",
			Type:   "bool",
			Offset: 16,
		},
		Field{
			Name:   "tuesday",
			Type:   "bool",
			Offset: 17,
		},
		Field{
			Name:   "wednesday",
			Type:   "bool",
			Offset: 18,
		},
		Field{
			Name:   "thursday",
			Type:   "bool",
			Offset: 19,
		},
		Field{
			Name:   "friday",
			Type:   "bool",
			Offset: 20,
		},
		Field{
			Name:   "saturday",
			Type:   "bool",
			Offset: 21,
		},
		Field{
			Name:   "sunday",
			Type:   "bool",
			Offset: 22,
		},
		Field{
			Name:   "start time",
			Type:   "HHmm",
			Offset: 23,
		},
		Field{
			Name:   "door",
			Type:   "uint8",
			Offset: 25,
		},
		Field{
			Name:   "task type",
			Type:   "uint8",
			Offset: 26,
		},
		Field{
			Name:   "more cards",
			Type:   "uint8",
			Offset: 27,
		},
	},
}

var RefreshTaskListRequest = Message{
	Name:    "refresh tasklist request",
	MsgType: 0xac,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "",
			Type:   "magic",
			Offset: 8,
		},
	},
}

var ClearTaskListRequest = Message{
	Name:    "clear tasklist request",
	MsgType: 0xa6,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "",
			Type:   "magic",
			Offset: 8,
		},
	},
}

var SetPCControlRequest = Message{
	Name:    "set pc control request",
	MsgType: 0xa0,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "",
			Type:   "magic",
			Offset: 8,
		},
		Field{
			Name:   "enable",
			Type:   "bool",
			Offset: 12,
		},
	},
}
