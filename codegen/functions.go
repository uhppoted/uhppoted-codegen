package codegen

import ()

type function struct {
	Name     string
	Args     []arg
	Request  request
	Response response
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
}
