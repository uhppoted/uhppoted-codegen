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
	ClearTimeProfiles,
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
var GetListenerAddrPort = functions.GetListenerAddrPort
var SetListenerAddrPort = functions.SetListenerAddrPort
var GetStatus = functions.GetStatus
var PutCard = functions.PutCard
var GetCards = functions.GetCards
var GetCard = functions.GetCard
var GetCardAtIndex = functions.GetCardAtIndex
var DeleteCard = functions.DeleteCard
var DeleteAllCards = functions.DeleteAllCards
var GetEvent = functions.GetEvent
var GetEventIndex = functions.GetEventIndex
var SetEventIndex = functions.SetEventIndex
var RecordSpecialEvents = functions.RecordSpecialEvents
var GetTimeProfile = functions.GetTimeProfile
var SetTimeProfile = functions.SetTimeProfile
var ClearTimeProfiles = functions.ClearTimeProfiles
var AddTask = functions.AddTask
var RefreshTaskList = functions.RefreshTaskList
var ClearTaskList = functions.ClearTaskList
var SetPCControl = functions.SetPCControl
var SetInterlock = functions.SetInterlock
var ActivateKeypads = functions.ActivateKeypads
var GetAntiPassback = functions.GetAntiPassback
var SetAntiPassback = functions.SetAntiPassback
var RestoreDefaultParameters = functions.RestoreDefaultParameters

// var AddTask = types.Function{
// 	Name: "add task",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 		{
// 			Name: "start date",
// 			Type: "date",
// 		},
// 		{
// 			Name: "end date",
// 			Type: "date",
// 		},
// 		{
// 			Name: "monday",
// 			Type: "bool",
// 		},
// 		{
// 			Name: "tuesday",
// 			Type: "bool",
// 		},
// 		{
// 			Name: "wednesday",
// 			Type: "bool",
// 		},
// 		{
// 			Name: "thursday",
// 			Type: "bool",
// 		},
// 		{
// 			Name: "friday",
// 			Type: "bool",
// 		},
// 		{
// 			Name: "saturday",
// 			Type: "bool",
// 		},
// 		{
// 			Name: "sunday",
// 			Type: "bool",
// 		},
// 		{
// 			Name: "start time",
// 			Type: "HHmm",
// 		},
// 		{
// 			Name: "door",
// 			Type: "uint8",
// 		},
// 		{
// 			Name: "task type",
// 			Type: "uint8",
// 		},
// 		{
// 			Name: "more cards",
// 			Type: "uint8",
// 		},
// 	},
// 	Request:  AddTaskRequest,
// 	Response: AddTaskResponse,
// }

// var RefreshTaskList = types.Function{
// 	Name: "refresh tasklist",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 	},
// 	Request:  RefreshTaskListRequest,
// 	Response: RefreshTaskListResponse,
// }

// var ClearTaskList = types.Function{
// 	Name: "clear tasklist",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 	},
// 	Request:  ClearTaskListRequest,
// 	Response: ClearTaskListResponse,
// }

// var SetPCControl = types.Function{
// 	Name: "set pc control",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 		{
// 			Name: "enable",
// 			Type: "bool",
// 		},
// 	},
// 	Request:  SetPCControlRequest,
// 	Response: SetPCControlResponse,
// }

// var SetInterlock = types.Function{
// 	Name: "set interlock",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 		{
// 			Name: "interlock",
// 			Type: "uint8",
// 		},
// 	},
// 	Request:  SetInterlockRequest,
// 	Response: SetInterlockResponse,
// }

// var ActivateKeypads = types.Function{
// 	Name: "activate keypads",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 		{
// 			Name: "reader 1",
// 			Type: "bool",
// 		},
// 		{
// 			Name: "reader 2",
// 			Type: "bool",
// 		},
// 		{
// 			Name: "reader 3",
// 			Type: "bool",
// 		},
// 		{
// 			Name: "reader 4",
// 			Type: "bool",
// 		},
// 	},
// 	Request:  ActivateKeypadsRequest,
// 	Response: ActivateKeypadsResponse,
// }

// var GetAntiPassback = types.Function{
// 	Name: "get antipassback",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 	},
// 	Request:  GetAntiPassbackRequest,
// 	Response: GetAntiPassbackResponse,
// }

// var SetAntiPassback = types.Function{
// 	Name: "set antipassback",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 		{
// 			Name: "antipassback",
// 			Type: "uint8",
// 		},
// 	},
// 	Request:  SetAntiPassbackRequest,
// 	Response: SetAntiPassbackResponse,
// }

// var RestoreDefaultParameters = types.Function{
// 	Name: "restore default parameters",
// 	Args: []types.Arg{
// 		{
// 			Name: "controller",
// 			Type: "controller",
// 		},
// 	},
// 	Request:  RestoreDefaultParametersRequest,
// 	Response: RestoreDefaultParametersResponse,
// }
