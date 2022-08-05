package codegen

import ()

type request struct {
	Name    string
	MsgType uint8
	Fields  []field
}

var requests = []request{
	GetControllerRequest,
}

var GetControllerRequest = request{
	Name:    "get controller request",
	MsgType: 0x94,
	Fields: []field{
		DeviceID,
	},
}
