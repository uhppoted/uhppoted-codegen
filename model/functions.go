package model

import ()

var Functions = []Function{
	GetController,
	SetIP,
	GetTime,
	SetTime,
	GetStatus,
	GetListener,
	SetListener,
}

var GetController = Function{
	Name: "get controller",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  &GetControllerRequest,
	Response: &GetControllerResponse,
}

var SetIP = Function{
	Name: "set IP",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "address",
			Type: "IPv4",
		},
		Arg{
			Name: "netmask",
			Type: "IPv4",
		},
		Arg{
			Name: "gateway",
			Type: "IPv4",
		},
	},
	Request: &SetIPRequest,
}

var GetTime = Function{
	Name: "get time",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  &GetTimeRequest,
	Response: &GetTimeResponse,
}

var SetTime = Function{
	Name: "set time",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "time",
			Type: "datetime",
		},
	},
	Request:  &SetTimeRequest,
	Response: &SetTimeResponse,
}

var GetStatus = Function{
	Name: "get status",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  &GetStatusRequest,
	Response: &GetStatusResponse,
}

var GetListener = Function{
	Name: "get listener",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  &GetListenerRequest,
	Response: &GetListenerResponse,
}

var SetListener = Function{
	Name: "set listener",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "address",
			Type: "IPv4",
		},
		Arg{
			Name: "port",
			Type: "uint16",
		},
	},
	Request:  &SetListenerRequest,
	Response: &SetListenerResponse,
}
