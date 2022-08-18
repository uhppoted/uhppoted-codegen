package codegen

var responses = []response{
	GetControllerResponse,
	GetTimeResponse,
	SetTimeResponse,
	GetStatusResponse,
}

var GetControllerResponse = response{
	Name:    "get controller response",
	MsgType: 0x94,
	Fields: []field{
		field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		field{
			Name:   "ip address",
			Type:   "IPv4",
			Offset: 8,
		},
		field{
			Name:   "subnet mask",
			Type:   "IPv4",
			Offset: 12,
		},
		field{
			Name:   "gateway",
			Type:   "IPv4",
			Offset: 16,
		},
		field{
			Name:   "MAC address",
			Type:   "MAC",
			Offset: 20,
		},
		field{
			Name:   "version",
			Type:   "version",
			Offset: 26,
		},
		field{
			Name:   "date",
			Type:   "date",
			Offset: 28,
		},
	},
}

var GetTimeResponse = response{
	Name:    "get time response",
	MsgType: 0x32,
	Fields: []field{
		field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		field{
			Name:   "datetime",
			Type:   "datetime",
			Offset: 8,
		},
	},
}

var SetTimeResponse = response{
	Name:    "set time response",
	MsgType: 0x30,
	Fields: []field{
		field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		field{
			Name:   "datetime",
			Type:   "datetime",
			Offset: 8,
		},
	},
}

var GetStatusResponse = response{
	Name:    "get status response",
	MsgType: 0x20,
	Fields: []field{
		field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		field{
			Name:   "system date",
			Type:   "shortdate",
			Offset: 51,
		},
		field{
			Name:   "system time",
			Type:   "time",
			Offset: 37,
		},
		field{
			Name:   "door 1 open",
			Type:   "bool",
			Offset: 28,
		},
		field{
			Name:   "door 2 open",
			Type:   "bool",
			Offset: 29,
		},
		field{
			Name:   "door 3 open",
			Type:   "bool",
			Offset: 30,
		},
		field{
			Name:   "door 4 open",
			Type:   "bool",
			Offset: 31,
		},
		field{
			Name:   "door 1 button",
			Type:   "bool",
			Offset: 32,
		},
		field{
			Name:   "door 2 button",
			Type:   "bool",
			Offset: 33,
		},
		field{
			Name:   "door 3 button",
			Type:   "bool",
			Offset: 34,
		},
		field{
			Name:   "door 4 button",
			Type:   "bool",
			Offset: 35,
		},
		field{
			Name:   "relays",
			Type:   "uint8",
			Offset: 49,
		},
		field{
			Name:   "inputs",
			Type:   "uint8",
			Offset: 50,
		},
		field{
			Name:   "system error",
			Type:   "uint8",
			Offset: 36,
		},
		field{
			Name:   "special info",
			Type:   "uint8",
			Offset: 48,
		},
		field{
			Name:   "event index",
			Type:   "uint32",
			Offset: 8,
		},
		field{
			Name:   "event type",
			Type:   "uint8",
			Offset: 12,
		},
		field{
			Name:   "event access granted",
			Type:   "bool",
			Offset: 13,
		},
		field{
			Name:   "event door",
			Type:   "uint8",
			Offset: 14,
		},
		field{
			Name:   "event direction",
			Type:   "uint8",
			Offset: 15,
		},
		field{
			Name:   "event card",
			Type:   "uint32",
			Offset: 16,
		},
		field{
			Name:   "event timestamp",
			Type:   "datetime",
			Offset: 20,
		},
		field{
			Name:   "event reason",
			Type:   "uint8",
			Offset: 27,
		},
		field{
			Name:   "sequence no",
			Type:   "uint32",
			Offset: 40,
		},
	},
}
