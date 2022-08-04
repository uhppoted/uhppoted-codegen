package codegen

import ()

type request struct {
	Name    string
	MsgType uint8
	Fields  []field
}

type field struct {
	Name   string
	Type   string
	Offset uint8
}

var requests = []request{
	GetAllControllersRequest,
	GetControllerRequest,
}

var DeviceID = field{
	Name:   "device id",
	Type:   "uint32",
	Offset: 4,
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
