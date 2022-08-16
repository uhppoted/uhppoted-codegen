package codegen

import ()

type response struct {
	Name    string  `json:"name"`
	MsgType uint8   `json:"type"`
	Fields  []field `json:"fields"`
}

var responses = []response{
	GetControllerResponse,
	GetTimeResponse,
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

var GetTimeResponse = response{
	Name:    "get time response",
	MsgType: 0x32,
	Fields: []field{
		DeviceID,
		field{
			Name:   "datetime",
			Type:   "datetime",
			Offset: 8,
		},
	},
}
