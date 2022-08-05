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
	},
}
