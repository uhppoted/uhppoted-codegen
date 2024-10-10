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
	SetTimeProfile,
	DeleteAllTimeProfiles,
	AddTask,
	RefreshTaskList,
	ClearTaskList,
	SetPCControl,
	SetInterlock,
	ActivateKeypads,
	SetDoorPasscodes,
	RestoreDefaultParameters,
}

var GetController = Function{
	Name: "get controller",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &GetControllerRequest,
	Response: &GetControllerResponse,
}

var SetIP = Function{
	Name: "set IP",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &GetTimeRequest,
	Response: &GetTimeResponse,
}

var SetTime = Function{
	Name: "set time",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &GetStatusRequest,
	Response: &GetStatusResponse,
}

var GetListener = Function{
	Name: "get listener",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &GetListenerRequest,
	Response: &GetListenerResponse,
}

var SetListener = Function{
	Name: "set listener",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
		Arg{
			Name: "address",
			Type: "IPv4",
		},
		Arg{
			Name: "port",
			Type: "uint16",
		},
		Arg{
			Name: "interval",
			Type: "uint8",
		},
	},
	Request:  &SetListenerRequest,
	Response: &SetListenerResponse,
}

var GetDoorControl = Function{
	Name: "get door control",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &GetCardsRequest,
	Response: &GetCardsResponse,
}

var GetCard = Function{
	Name: "get card",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
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
		Arg{
			Name: "PIN",
			Type: "pin",
		},
	},
	Request:  &PutCardRequest,
	Response: &PutCardResponse,
}

var DeleteCard = Function{
	Name: "delete card",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &DeleteAllCardsRequest,
	Response: &DeleteAllCardsResponse,
}

var GetEvent = Function{
	Name: "get event",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &GetEventIndexRequest,
	Response: &GetEventIndexResponse,
}

var SetEventIndex = Function{
	Name: "set event index",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
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
			Name: "controller",
			Type: "controller",
		},
		Arg{
			Name: "profile id",
			Type: "uint8",
		},
	},
	Request:  &GetTimeProfileRequest,
	Response: &GetTimeProfileResponse,
}

var SetTimeProfile = Function{
	Name: "set time profile",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
		Arg{
			Name: "profile id",
			Type: "uint8",
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
			Name: "monday",
			Type: "bool",
		},
		Arg{
			Name: "tuesday",
			Type: "bool",
		},
		Arg{
			Name: "wednesday",
			Type: "bool",
		},
		Arg{
			Name: "thursday",
			Type: "bool",
		},
		Arg{
			Name: "friday",
			Type: "bool",
		},
		Arg{
			Name: "saturday",
			Type: "bool",
		},
		Arg{
			Name: "sunday",
			Type: "bool",
		},
		Arg{
			Name: "segment 1 start",
			Type: "HHmm",
		},
		Arg{
			Name: "segment 1 end",
			Type: "HHmm",
		},
		Arg{
			Name: "segment 2 start",
			Type: "HHmm",
		},
		Arg{
			Name: "segment 2 end",
			Type: "HHmm",
		},
		Arg{
			Name: "segment 3 start",
			Type: "HHmm",
		},
		Arg{
			Name: "segment 3 end",
			Type: "HHmm",
		},
		Arg{
			Name: "linked profile id",
			Type: "uint8",
		},
	},
	Request:  &SetTimeProfileRequest,
	Response: &SetTimeProfileResponse,
}

var DeleteAllTimeProfiles = Function{
	Name: "delete all time profiles",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &DeleteAllTimeProfilesRequest,
	Response: &DeleteAllTimeProfilesResponse,
}

var AddTask = Function{
	Name: "add task",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
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
			Name: "monday",
			Type: "bool",
		},
		Arg{
			Name: "tuesday",
			Type: "bool",
		},
		Arg{
			Name: "wednesday",
			Type: "bool",
		},
		Arg{
			Name: "thursday",
			Type: "bool",
		},
		Arg{
			Name: "friday",
			Type: "bool",
		},
		Arg{
			Name: "saturday",
			Type: "bool",
		},
		Arg{
			Name: "sunday",
			Type: "bool",
		},
		Arg{
			Name: "start time",
			Type: "HHmm",
		},
		Arg{
			Name: "door",
			Type: "uint8",
		},
		Arg{
			Name: "task type",
			Type: "uint8",
		},
		Arg{
			Name: "more cards",
			Type: "uint8",
		},
	},
	Request:  &AddTaskRequest,
	Response: &AddTaskResponse,
}

var RefreshTaskList = Function{
	Name: "refresh tasklist",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &RefreshTaskListRequest,
	Response: &RefreshTaskListResponse,
}

var ClearTaskList = Function{
	Name: "clear tasklist",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &ClearTaskListRequest,
	Response: &ClearTaskListResponse,
}

var SetPCControl = Function{
	Name: "set pc control",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
		Arg{
			Name: "enable",
			Type: "bool",
		},
	},
	Request:  &SetPCControlRequest,
	Response: &SetPCControlResponse,
}

var SetInterlock = Function{
	Name: "set interlock",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
		Arg{
			Name: "interlock",
			Type: "uint8",
		},
	},
	Request:  &SetInterlockRequest,
	Response: &SetInterlockResponse,
}

var ActivateKeypads = Function{
	Name: "activate keypads",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
		Arg{
			Name: "reader 1",
			Type: "bool",
		},
		Arg{
			Name: "reader 2",
			Type: "bool",
		},
		Arg{
			Name: "reader 3",
			Type: "bool",
		},
		Arg{
			Name: "reader 4",
			Type: "bool",
		},
	},
	Request:  &ActivateKeypadsRequest,
	Response: &ActivateKeypadsResponse,
}

var SetDoorPasscodes = Function{
	Name: "set door passcodes",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
		Arg{
			Name: "door",
			Type: "uint8",
		},
		Arg{
			Name: "passcode 1",
			Type: "uint32",
		},
		Arg{
			Name: "passcode 2",
			Type: "uint32",
		},
		Arg{
			Name: "passcode 3",
			Type: "uint32",
		},
		Arg{
			Name: "passcode 4",
			Type: "uint32",
		},
	},
	Request:  &SetDoorPasscodesRequest,
	Response: &SetDoorPasscodesResponse,
}

var RestoreDefaultParameters = Function{
	Name: "restore default parameters",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &RestoreDefaultParametersRequest,
	Response: &RestoreDefaultParametersResponse,
}
