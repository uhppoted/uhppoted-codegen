package model

import (
	"github.com/uhppoted/uhppoted-codegen/model/functions"
	"github.com/uhppoted/uhppoted-codegen/model/types"
)

var Functions = []types.Function{
	GetController,
	SetIPv4,
	GetTime,
	SetTime,
	GetListener,
	SetListener,
	GetDoor,
	SetDoor,
	OpenDoor,
	GetStatus,
	GetCards,
	GetCard,
	GetCardAtIndex,
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
	GetAntiPassback,
	SetAntiPassback,
	RestoreDefaultParameters,
}

var GetController = functions.GetController
var SetIPv4 = functions.SetIPv4
var GetTime = functions.GetTime
var SetTime = functions.SetTime
var GetDoor = functions.GetDoor
var SetDoor = functions.SetDoor
var SetDoorPasscodes = functions.SetDoorPasscodes
var OpenDoor = functions.OpenDoor
var GetListener = functions.GetListener
var SetListener = functions.SetListener
var PutCard = functions.PutCard
var GetCards = functions.GetCards
var GetCard = functions.GetCard
var GetCardAtIndex = functions.GetCardAtIndex
var DeleteCard = functions.DeleteCard
var DeleteAllCards = functions.DeleteAllCards
var GetEventIndex = functions.GetEventIndex
var SetEventIndex = functions.SetEventIndex

// var OpenDoor = types.Function{
// 	Name:        "open door",
// 	Description: []string{"Unlocks a door controlled by an access controller.",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 		{
// 			Name: "door",
// 			Type: "uint8",
// 		},
// 	},
// 	Request:  OpenDoorRequest.Message,
// 	Response: OpenDoorResponse.Message,
// }

var GetStatus = types.Function{
	Name:        "get status",
	Description: []string{"Retrieves the system status from an access controller."},
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  GetStatusRequest.Message,
	Response: GetStatusResponse.Message,
}

var GetEvent = types.Function{
	Name: "get event",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "event index",
			Type: "uint32",
		},
	},
	Request:  GetEventRequest.Message,
	Response: GetEventResponse.Message,
}

// var SetEventIndex = types.Function{
// 	Name: "set event index",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 		{
// 			Name: "event index",
// 			Type: "uint32",
// 		},
// 	},
// 	Request:  SetEventIndexRequest,
// 	Response: SetEventIndexResponse,
// }

var RecordSpecialEvents = types.Function{
	Name: "record special events",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "enable",
			Type: "bool",
		},
	},
	Request:  RecordSpecialEventsRequest,
	Response: RecordSpecialEventsResponse,
}

var GetTimeProfile = types.Function{
	Name: "get time profile",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "profile id",
			Type: "uint8",
		},
	},
	Request:  GetTimeProfileRequest,
	Response: GetTimeProfileResponse,
}

var SetTimeProfile = types.Function{
	Name: "set time profile",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "profile id",
			Type: "uint8",
		},
		{
			Name: "start date",
			Type: "date",
		},
		{
			Name: "end date",
			Type: "date",
		},
		{
			Name: "monday",
			Type: "bool",
		},
		{
			Name: "tuesday",
			Type: "bool",
		},
		{
			Name: "wednesday",
			Type: "bool",
		},
		{
			Name: "thursday",
			Type: "bool",
		},
		{
			Name: "friday",
			Type: "bool",
		},
		{
			Name: "saturday",
			Type: "bool",
		},
		{
			Name: "sunday",
			Type: "bool",
		},
		{
			Name: "segment 1 start",
			Type: "HHmm",
		},
		{
			Name: "segment 1 end",
			Type: "HHmm",
		},
		{
			Name: "segment 2 start",
			Type: "HHmm",
		},
		{
			Name: "segment 2 end",
			Type: "HHmm",
		},
		{
			Name: "segment 3 start",
			Type: "HHmm",
		},
		{
			Name: "segment 3 end",
			Type: "HHmm",
		},
		{
			Name: "linked profile id",
			Type: "uint8",
		},
	},
	Request:  SetTimeProfileRequest,
	Response: SetTimeProfileResponse,
}

var DeleteAllTimeProfiles = types.Function{
	Name: "delete all time profiles",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  DeleteAllTimeProfilesRequest,
	Response: DeleteAllTimeProfilesResponse,
}

var AddTask = types.Function{
	Name: "add task",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "start date",
			Type: "date",
		},
		{
			Name: "end date",
			Type: "date",
		},
		{
			Name: "monday",
			Type: "bool",
		},
		{
			Name: "tuesday",
			Type: "bool",
		},
		{
			Name: "wednesday",
			Type: "bool",
		},
		{
			Name: "thursday",
			Type: "bool",
		},
		{
			Name: "friday",
			Type: "bool",
		},
		{
			Name: "saturday",
			Type: "bool",
		},
		{
			Name: "sunday",
			Type: "bool",
		},
		{
			Name: "start time",
			Type: "HHmm",
		},
		{
			Name: "door",
			Type: "uint8",
		},
		{
			Name: "task type",
			Type: "uint8",
		},
		{
			Name: "more cards",
			Type: "uint8",
		},
	},
	Request:  AddTaskRequest,
	Response: AddTaskResponse,
}

var RefreshTaskList = types.Function{
	Name: "refresh tasklist",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  RefreshTaskListRequest,
	Response: RefreshTaskListResponse,
}

var ClearTaskList = types.Function{
	Name: "clear tasklist",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  ClearTaskListRequest,
	Response: ClearTaskListResponse,
}

var SetPCControl = types.Function{
	Name: "set pc control",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "enable",
			Type: "bool",
		},
	},
	Request:  SetPCControlRequest,
	Response: SetPCControlResponse,
}

var SetInterlock = types.Function{
	Name: "set interlock",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "interlock",
			Type: "uint8",
		},
	},
	Request:  SetInterlockRequest,
	Response: SetInterlockResponse,
}

var ActivateKeypads = types.Function{
	Name: "activate keypads",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "reader 1",
			Type: "bool",
		},
		{
			Name: "reader 2",
			Type: "bool",
		},
		{
			Name: "reader 3",
			Type: "bool",
		},
		{
			Name: "reader 4",
			Type: "bool",
		},
	},
	Request:  ActivateKeypadsRequest,
	Response: ActivateKeypadsResponse,
}

// var SetDoorPasscodes = types.Function{
// 	Name:        "set door passcodes",
// 	Description: []string{"Sets up to 4 passcodes for a controller door.",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 		{
// 			Name: "door",
// 			Type: "uint8",
// 		},
// 		{
// 			Name: "passcode 1",
// 			Type: "pin",
// 		},
// 		{
// 			Name: "passcode 2",
// 			Type: "pin",
// 		},
// 		{
// 			Name: "passcode 3",
// 			Type: "pin",
// 		},
// 		{
// 			Name: "passcode 4",
// 			Type: "pin",
// 		},
// 	},
// 	Request:  SetDoorPasscodesRequest.Message,
// 	Response: SetDoorPasscodesResponse.Message,
// }

var GetAntiPassback = types.Function{
	Name: "get antipassback",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  GetAntiPassbackRequest,
	Response: GetAntiPassbackResponse,
}

var SetAntiPassback = types.Function{
	Name: "set antipassback",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "antipassback",
			Type: "uint8",
		},
	},
	Request:  SetAntiPassbackRequest,
	Response: SetAntiPassbackResponse,
}

var RestoreDefaultParameters = types.Function{
	Name: "restore default parameters",
	Args: []types.Arg{
		{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  RestoreDefaultParametersRequest,
	Response: RestoreDefaultParametersResponse,
}
