package codegen

import ()

var functions = []function{
	GetController,
	SetIP,
	GetTime,
	SetTime,
	GetStatus,
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

var GetTime = function{
	Name: "get time",
	Args: []arg{
		arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  &GetTimeRequest,
	Response: &GetTimeResponse,
}

var SetTime = function{
	Name: "set time",
	Args: []arg{
		arg{
			Name: "device id",
			Type: "uint32",
		},
		arg{
			Name: "time",
			Type: "datetime",
		},
	},
	Request:  &SetTimeRequest,
	Response: &SetTimeResponse,
}

var GetStatus = function{
	Name: "get status",
	Args: []arg{
		arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  &GetStatusRequest,
	Response: &GetStatusResponse,
}
