package codegen

import ()

type request struct {
	Name    string
	Args    []arg
	MsgType uint8
}

var requests = []request{
	GetAllControllersRequest,
	GetControllerRequest,
}

var GetAllControllersRequest = request{
	Name:    "get all controllers",
	Args:    []arg{},
	MsgType: 0x94,
}

var GetControllerRequest = request{
	Name: "get controller",
	Args: []arg{
		arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	MsgType: 0x94,
}
