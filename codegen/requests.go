package codegen

import ()

type request struct {
	Name    string
	MsgType uint8
	Fields  []field
}

var requests = []request{
	GetControllerRequest,
	SetIPRequest,
	GetTimeRequest,
}

var GetControllerRequest = request{
	Name:    "get controller request",
	MsgType: 0x94,
	Fields: []field{
		DeviceID,
	},
}

var SetIPRequest = request{
	Name:    "set IP request",
	MsgType: 0x94,
	Fields: []field{
		DeviceID,
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
		DeviceID,
	},
}
