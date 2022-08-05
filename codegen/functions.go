package codegen

import ()

type function struct {
	Name     string
	Args     []arg
	Request  request
	Response response
	Wait     uint32
}

var functions = []function{
	GetController,
}

var GetController = function{
	Name: "get controller",
	Args: []arg{
		arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  GetControllerRequest,
	Response: GetControllerResponse,
	Wait:     0,
}
