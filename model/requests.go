package model

var Requests = []Request{
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
}

var GetControllerRequest = Request{
	Name:    "get controller request",
	MsgType: 0x94,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
	TestData: []uint8{
		0x17, 0x94, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var SetIPRequest = Request{
	Name:    "set IP request",
	MsgType: 0x94,
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
	TestData: []uint8{
		0x17, 0x94, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xc0, 0xa8, 0x01, 0x64, 0xff, 0xff, 0xff, 0x00,
		0xc0, 0xa8, 0x01, 0x01, 0x55, 0xaa, 0xaa, 0x55, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var GetTimeRequest = Request{
	Name:    "get time request",
	MsgType: 0x32,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
	TestData: []uint8{
		0x17, 0x32, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var SetTimeRequest = Request{
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
	TestData: []uint8{
		0x17, 0x30, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x20, 0x22, 0x08, 0x23, 0x09, 0x49, 0x03, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var GetStatusRequest = Request{
	Name:    "get status request",
	MsgType: 0x20,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
	TestData: []uint8{
		0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var GetListenerRequest = Request{
	Name:    "get listener request",
	MsgType: 0x92,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
	TestData: []uint8{
		0x17, 0x92, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var SetListenerRequest = Request{
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
	TestData: []uint8{
		0x17, 0x90, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xc0, 0xa8, 0x01, 0x64, 0x61, 0xea, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var GetDoorControlRequest = Request{
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
	TestData: []uint8{
		0x17, 0x82, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var SetDoorControlRequest = Request{
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
	TestData: []uint8{
		0x17, 0x80, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x02, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var OpenDoorRequest = Request{
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
	TestData: []uint8{
		0x17, 0x40, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var GetCardsRequest = Request{
	Name:    "get cards request",
	MsgType: 0x58,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
	TestData: []uint8{
		0x17, 0x58, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var GetCardRequest = Request{
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
	TestData: []uint8{
		0x17, 0x5a, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xa2, 0x98, 0x7c, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var GetCardByIndexRequest = Request{
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
	TestData: []uint8{
		0x17, 0x5c, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var PutCardRequest = Request{
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
	TestData: []uint8{
		0x17, 0x50, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xa2, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x01, 0x01,
		0x20, 0x22, 0x12, 0x31, 0x00, 0x01, 0x1d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var DeleteCardRequest = Request{
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

	TestData: []uint8{
		0x17, 0x52, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xa2, 0x98, 0x7c, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}

var DeleteAllCardsRequest = Request{
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

	TestData: []uint8{
		0x17, 0x54, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x55, 0xaa, 0xaa, 0x55, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	},
}
