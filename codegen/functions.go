package codegen

import ()

type function struct {
	Name    string
	Args    []arg
	Request request
	Timeout uint32
}

type arg struct {
	Name string
	Type string
}

var functions = []function{
	GetAllControllers,
	GetController,
}

var GetAllControllers = function{
	Name:    "get all controllers",
	Args:    []arg{},
	Request: GetAllControllersRequest,
	Timeout: 5000,
}

var GetController = function{
	Name: "get controller",
	Args: []arg{
		arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request: GetControllerRequest,
	Timeout: 1000,
}
