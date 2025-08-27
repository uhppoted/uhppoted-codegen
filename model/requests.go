package model

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/types"
)

var Requests = []types.Message{
	GetControllerRequest.Message,
	SetIPv4Request.Message,
	GetTimeRequest.Message,
	SetTimeRequest.Message,
	GetListenerRequest.Message,
	SetListenerRequest.Message,
	// GetListenerAddrPortRequest.Message,
	// SetListenerAddrPortRequest.Message,
	GetDoorRequest.Message,
	SetDoorRequest.Message,
	SetDoorPasscodesRequest.Message,
	OpenDoorRequest.Message,
	GetStatusRequest.Message,
	GetCardsRequest.Message,
	GetCardRequest.Message,
	GetCardAtIndexRequest.Message,
	PutCardRequest.Message,
	DeleteCardRequest.Message,
	DeleteAllCardsRequest.Message,
	GetEventRequest.Message,
	GetEventIndexRequest.Message,
	SetEventIndexRequest.Message,
	RecordSpecialEventsRequest.Message,
	GetTimeProfileRequest.Message,
	SetTimeProfileRequest.Message,
	ClearTimeProfilesRequest.Message,
	AddTaskRequest.Message,
	RefreshTaskListRequest.Message,
	ClearTaskListRequest.Message,
	SetPCControlRequest.Message,
	SetInterlockRequest.Message,
	ActivateKeypadsRequest.Message,
	GetAntiPassbackRequest.Message,
	SetAntiPassbackRequest,
	RestoreDefaultParametersRequest,
}

var GetControllerRequest = requests.GetControllerRequest
var SetIPv4Request = requests.SetIPv4Request
var GetTimeRequest = requests.GetTimeRequest
var SetTimeRequest = requests.SetTimeRequest
var GetListenerRequest = requests.GetListenerRequest
var SetListenerRequest = requests.SetListenerRequest
var GetListenerAddrPortRequest = requests.GetListenerAddrPortRequest
var SetListenerAddrPortRequest = requests.SetListenerAddrPortRequest
var GetDoorRequest = requests.GetDoorRequest
var SetDoorRequest = requests.SetDoorRequest
var SetDoorPasscodesRequest = requests.SetDoorPasscodesRequest
var OpenDoorRequest = requests.OpenDoorRequest
var GetStatusRequest = requests.GetStatusRequest
var GetCardsRequest = requests.GetCardsRequest
var GetCardRequest = requests.GetCardRequest
var GetCardAtIndexRequest = requests.GetCardAtIndexRequest
var PutCardRequest = requests.PutCardRequest
var DeleteCardRequest = requests.DeleteCardRequest
var DeleteAllCardsRequest = requests.DeleteAllCardsRequest
var GetEventRequest = requests.GetEventRequest
var GetEventIndexRequest = requests.GetEventIndexRequest
var SetEventIndexRequest = requests.SetEventIndexRequest
var RecordSpecialEventsRequest = requests.RecordSpecialEventsRequest
var GetTimeProfileRequest = requests.GetTimeProfileRequest
var SetTimeProfileRequest = requests.SetTimeProfileRequest
var ClearTimeProfilesRequest = requests.ClearTimeProfilesRequest
var AddTaskRequest = requests.AddTaskRequest
var RefreshTaskListRequest = requests.RefreshTaskListRequest
var ClearTaskListRequest = requests.ClearTaskListRequest
var SetPCControlRequest = requests.SetPCControlRequest
var SetInterlockRequest = requests.SetInterlockRequest
var ActivateKeypadsRequest = requests.ActivateKeypadsRequest
var GetAntiPassbackRequest = requests.GetAntiPassbackRequest

// var AddTaskRequest = types.Message{
// 	Name:    "add task request",
// 	MsgType: 0xa8,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "start date",
// 			Type:   "date",
// 			Offset: 8,
// 		},
// 		types.Field{
// 			Name:   "end date",
// 			Type:   "date",
// 			Offset: 12,
// 		},
// 		types.Field{
// 			Name:   "monday",
// 			Type:   "bool",
// 			Offset: 16,
// 		},
// 		types.Field{
// 			Name:   "tuesday",
// 			Type:   "bool",
// 			Offset: 17,
// 		},
// 		types.Field{
// 			Name:   "wednesday",
// 			Type:   "bool",
// 			Offset: 18,
// 		},
// 		types.Field{
// 			Name:   "thursday",
// 			Type:   "bool",
// 			Offset: 19,
// 		},
// 		types.Field{
// 			Name:   "friday",
// 			Type:   "bool",
// 			Offset: 20,
// 		},
// 		types.Field{
// 			Name:   "saturday",
// 			Type:   "bool",
// 			Offset: 21,
// 		},
// 		types.Field{
// 			Name:   "sunday",
// 			Type:   "bool",
// 			Offset: 22,
// 		},
// 		types.Field{
// 			Name:   "start time",
// 			Type:   "HHmm",
// 			Offset: 23,
// 		},
// 		types.Field{
// 			Name:   "door",
// 			Type:   "uint8",
// 			Offset: 25,
// 		},
// 		types.Field{
// 			Name:   "task type",
// 			Type:   "uint8",
// 			Offset: 26,
// 		},
// 		types.Field{
// 			Name:   "more cards",
// 			Type:   "uint8",
// 			Offset: 27,
// 		},
// 	},
// }

// var RefreshTaskListRequest = types.Message{
// 	Name:    "refresh tasklist request",
// 	MsgType: 0xac,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "",
// 			Type:   "magic",
// 			Offset: 8,
// 		},
// 	},
// }

// var ClearTaskListRequest = types.Message{
// 	Name:    "clear tasklist request",
// 	MsgType: 0xa6,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "",
// 			Type:   "magic",
// 			Offset: 8,
// 		},
// 	},
// }

// var SetPCControlRequest = types.Message{
// 	Name:    "set pc control request",
// 	MsgType: 0xa0,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "",
// 			Type:   "magic",
// 			Offset: 8,
// 		},
// 		types.Field{
// 			Name:   "enable",
// 			Type:   "bool",
// 			Offset: 12,
// 		},
// 	},
// }

// var SetInterlockRequest = types.Message{
// 	Name:    "set interlock request",
// 	MsgType: 0xa2,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "interlock",
// 			Type:   "uint8",
// 			Offset: 8,
// 		},
// 	},
// }

// var ActivateKeypadsRequest = types.Message{
// 	Name:    "activate keypads request",
// 	MsgType: 0xa4,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "reader 1",
// 			Type:   "bool",
// 			Offset: 8,
// 		},
// 		types.Field{
// 			Name:   "reader 2",
// 			Type:   "bool",
// 			Offset: 9,
// 		},
// 		types.Field{
// 			Name:   "reader 3",
// 			Type:   "bool",
// 			Offset: 10,
// 		},
// 		types.Field{
// 			Name:   "reader 4",
// 			Type:   "bool",
// 			Offset: 11,
// 		},
// 	},
// }

// var GetAntiPassbackRequest = types.Message{
// 	Name:    "get antipassback request",
// 	MsgType: 0x86,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 	},
// }

var SetAntiPassbackRequest = types.Message{
	Name:    "set antipassback request",
	MsgType: 0x84,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "antipassback",
			Type:   "uint8",
			Offset: 8,
		},
	},
}

var RestoreDefaultParametersRequest = types.Message{
	Name:    "restore default parameters request",
	MsgType: 0xc8,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "",
			Type:   "magic",
			Offset: 8,
		},
	},
}
