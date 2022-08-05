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
	GetAllControllers,
	GetController,
}

var GetAllControllers = function{
	Name:    "get all controllers",
	Args:    []arg{},
	Request: GetAllControllersRequest,
	Wait:    2500,
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
