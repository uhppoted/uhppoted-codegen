package codegen

var requests = []request{
	GetControllerRequest,
	SetIPRequest,
	GetTimeRequest,
	SetTimeRequest,
}

var GetControllerRequest = request{
	Name:    "get controller request",
	MsgType: 0x94,
	Fields: []field{
		field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
}

var SetIPRequest = request{
	Name:    "set IP request",
	MsgType: 0x94,
	Fields: []field{
		field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		field{
			Name:   "address",
			Type:   "IPv4",
			Offset: 8,
		},
		field{
			Name:   "netmask",
			Type:   "IPv4",
			Offset: 12,
		},
		field{
			Name:   "gateway",
			Type:   "IPv4",
			Offset: 16,
		},
		field{
			Name:   "",
			Type:   "magic",
			Offset: 20,
		},
	},
}

var GetTimeRequest = request{
	Name:    "get time request",
	MsgType: 0x32,
	Fields: []field{
		field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
	},
}

var SetTimeRequest = request{
	Name:    "set time request",
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
