package codegen

import ()

type function struct {
	Name string
	Args []arg
}

var functions = []function{
	GetAllControllers,
	GetController,
}

var GetAllControllers = function{
	Name: "get all controllers",
	Args: []arg{},
}

var GetController = function{
	Name: "get controller",
	Args: []arg{
		arg{
			Name: "device id",
			Type: "uint32",
		},
	},
}
