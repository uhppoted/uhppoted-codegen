package model

var Requests = []Request{
	GetControllerRequest,
	SetIPRequest,
	GetTimeRequest,
	SetTimeRequest,
	GetStatusRequest,
	GetListenerRequest,
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
}
