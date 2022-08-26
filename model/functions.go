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
	GetDoorControl,
	SetDoorControl,
	OpenDoor,
	GetCards,
	GetCard,
	GetCardByIndex,
	PutCard,
	DeleteCard,
	DeleteAllCards,
	GetEvent,
	GetEventIndex,
	SetEventIndex,
	RecordSpecialEvents,
	GetTimeProfile,
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

var GetDoorControl = Function{
	Name: "get door control",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "door",
			Type: "uint8",
		},
	},
	Request:  &GetDoorControlRequest,
	Response: &GetDoorControlResponse,
}

var SetDoorControl = Function{
	Name: "set door control",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "door",
			Type: "uint8",
		},
		Arg{
			Name: "mode",
			Type: "uint8",
		},
		Arg{
			Name: "delay",
			Type: "uint8",
		},
	},
	Request:  &SetDoorControlRequest,
	Response: &SetDoorControlResponse,
}

var OpenDoor = Function{
	Name: "open door",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "door",
			Type: "uint8",
		},
	},
	Request:  &OpenDoorRequest,
	Response: &OpenDoorResponse,
}

var GetCards = Function{
	Name: "get cards",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  &GetCardsRequest,
	Response: &GetCardsResponse,
}

var GetCard = Function{
	Name: "get card",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "card number",
			Type: "uint32",
		},
	},
	Request:  &GetCardRequest,
	Response: &GetCardResponse,
}

var GetCardByIndex = Function{
	Name: "get card by index",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "card index",
			Type: "uint32",
		},
	},
	Request:  &GetCardByIndexRequest,
	Response: &GetCardByIndexResponse,
}

var PutCard = Function{
	Name: "put card",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "card number",
			Type: "uint32",
		},
		Arg{
			Name: "start date",
			Type: "date",
		},
		Arg{
			Name: "end date",
			Type: "date",
		},
		Arg{
			Name: "door 1",
			Type: "uint8",
		},
		Arg{
			Name: "door 2",
			Type: "uint8",
		},
		Arg{
			Name: "door 3",
			Type: "uint8",
		},
		Arg{
			Name: "door 4",
			Type: "uint8",
		},
	},
	Request:  &PutCardRequest,
	Response: &PutCardResponse,
}

var DeleteCard = Function{
	Name: "delete card",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "card number",
			Type: "uint32",
		},
	},
	Request:  &DeleteCardRequest,
	Response: &DeleteCardResponse,
}

var DeleteAllCards = Function{
	Name: "delete all cards",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  &DeleteAllCardsRequest,
	Response: &DeleteAllCardsResponse,
}

var GetEvent = Function{
	Name: "get event",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "event index",
			Type: "uint32",
		},
	},
	Request:  &GetEventRequest,
	Response: &GetEventResponse,
}

var GetEventIndex = Function{
	Name: "get event index",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
	},
	Request:  &GetEventIndexRequest,
	Response: &GetEventIndexResponse,
}

var SetEventIndex = Function{
	Name: "set event index",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "event index",
			Type: "uint32",
		},
	},
	Request:  &SetEventIndexRequest,
	Response: &SetEventIndexResponse,
}

var RecordSpecialEvents = Function{
	Name: "record special events",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "enable",
			Type: "bool",
		},
	},
	Request:  &RecordSpecialEventsRequest,
	Response: &RecordSpecialEventsResponse,
}

var GetTimeProfile = Function{
	Name: "get time profile",
	Args: []Arg{
		Arg{
			Name: "device id",
			Type: "uint32",
		},
		Arg{
			Name: "profile id",
			Type: "uint8",
		},
	},
	Request:  &GetTimeProfileRequest,
	Response: &GetTimeProfileResponse,
}
