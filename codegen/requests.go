package codegen

import ()

type request struct {
	Name    string
	MsgType uint8
	Fields  []field
}

var requests = []request{
	GetAllControllersRequest,
	GetControllerRequest,
}

var GetAllControllersRequest = request{
	Name:    "get all controllers request",
	MsgType: 0x94,
	Fields:  []field{},
}

var GetControllerRequest = request{
	Name:    "get controller request",
	MsgType: 0x94,
	Fields: []field{
		DeviceID,
	},
}
