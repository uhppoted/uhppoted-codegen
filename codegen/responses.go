package codegen

import ()

type response struct {
	Name    string
	MsgType uint8
	Fields  []field
}

var responses = []response{
	GetControllerResponse,
}

var GetControllerResponse = response{
	Name:    "get controller response",
	MsgType: 0x94,
	Fields: []field{
		DeviceID,

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
