package codegen

var responses = []response{
	GetControllerResponse,
	GetTimeResponse,
	SetTimeResponse,
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
