package codegen

import ()

type function struct {
	Name     string
	Args     []arg
	Request  *request
	Response *response
}

var functions = []function{
	GetController,
	SetIP,
}

var GetController = function{
	Name: "get controller",
	Args: []arg{
		arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  &GetControllerRequest,
	Response: &GetControllerResponse,
}

var SetIP = function{
	Name: "set IP",
	Args: []arg{
		arg{
			Name: "device id",
			Type: "uint32",
		},
		arg{
			Name: "address",
			Type: "IPv4",
		},
		arg{
			Name: "netmask",
			Type: "IPv4",
		},
		arg{
			Name: "gateway",
			Type: "IPv4",
		},
	},
	Request: &SetIPRequest,
}
