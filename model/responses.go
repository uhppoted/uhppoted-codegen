package model

var Responses = []Response{
	GetControllerResponse,
	GetTimeResponse,
	SetTimeResponse,
	GetStatusResponse,
	GetListenerResponse,
	SetListenerResponse,
}

var GetControllerResponse = Response{
	Name:    "get controller response",
	MsgType: 0x94,
	Fields: []Field{
		Field{
			Name:   "device id",
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

var GetTimeResponse = Response{
	Name:    "get time response",
	MsgType: 0x32,
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

var SetTimeResponse = Response{
	Name:    "set time response",
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

var GetStatusResponse = Response{
	Name:    "get status response",
	MsgType: 0x20,
	Fields: []Field{
		Field{
			Name:   "device id",
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
			Type:   "datetime",
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

var GetListenerResponse = Response{
	Name:    "get listener response",
	MsgType: 0x92,
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

var SetListenerResponse = Response{
	Name:    "set listener response",
	MsgType: 0x90,
	Fields: []Field{
		Field{
			Name:   "device id",
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
