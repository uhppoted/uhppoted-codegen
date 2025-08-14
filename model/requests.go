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
	GetDoorRequest.Message,
	SetDoorRequest.Message,
	OpenDoorRequest.Message,
	GetStatusRequest.Message,
	GetCardsRequest.Message,
	GetCardRequest.Message,
	GetCardAtIndexRequest.Message,
	PutCardRequest.Message,
	DeleteCardRequest.Message,
	DeleteAllCardsRequest.Message,
	GetEventRequest,
	GetEventIndexRequest,
	SetEventIndexRequest,
	RecordSpecialEventsRequest,
	GetTimeProfileRequest,
	SetTimeProfileRequest,
	DeleteAllTimeProfilesRequest,
	AddTaskRequest,
	RefreshTaskListRequest,
	ClearTaskListRequest,
	SetPCControlRequest,
	SetInterlockRequest,
	ActivateKeypadsRequest,
	SetDoorPasscodesRequest,
	GetAntiPassbackRequest,
	SetAntiPassbackRequest,
	RestoreDefaultParametersRequest,
}

var GetControllerRequest = requests.GetControllerRequest
var SetIPv4Request = requests.SetIPv4Request
var GetTimeRequest = requests.GetTimeRequest
var SetTimeRequest = requests.SetTimeRequest
var GetListenerRequest = requests.GetListenerRequest
var SetListenerRequest = requests.SetListenerRequest
var SetListenerAddrPortRequest = requests.SetListenerAddrPortRequest
var GetDoorRequest = requests.GetDoorRequest
var SetDoorRequest = requests.SetDoorRequest
var OpenDoorRequest = requests.OpenDoorRequest
var GetStatusRequest = requests.GetStatusRequest
var GetCardsRequest = requests.GetCardsRequest
var GetCardRequest = requests.GetCardRequest
var GetCardAtIndexRequest = requests.GetCardAtIndexRequest
var PutCardRequest = requests.PutCardRequest
var DeleteCardRequest = requests.DeleteCardRequest
var DeleteAllCardsRequest = requests.DeleteAllCardsRequest

var GetEventRequest = types.Message{
	Name:    "get event request",
	MsgType: 0xb0,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "event index",
			Type:   "uint32",
			Offset: 8,
		},
	},
}

var GetEventIndexRequest = types.Message{
	Name:    "get event index request",
	MsgType: 0xb4,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
	},
}

var SetEventIndexRequest = types.Message{
	Name:    "set event index request",
	MsgType: 0xb2,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "event index",
			Type:   "uint32",
			Offset: 8,
		},
		types.Field{
			Name:   "",
			Type:   "magic",
			Offset: 12,
		},
	},
}

var RecordSpecialEventsRequest = types.Message{
	Name:    "record special events request",
	MsgType: 0x8e,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "enable",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetTimeProfileRequest = types.Message{
	Name:    "get time profile request",
	MsgType: 0x98,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "profile id",
			Type:   "uint8",
			Offset: 8,
		},
	},
}

var SetTimeProfileRequest = types.Message{
	Name:    "set time profile request",
	MsgType: 0x88,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "profile id",
			Type:   "uint8",
			Offset: 8,
		},
		types.Field{
			Name:   "start date",
			Type:   "date",
			Offset: 9,
		},
		types.Field{
			Name:   "end date",
			Type:   "date",
			Offset: 13,
		},
		types.Field{
			Name:   "monday",
			Type:   "bool",
			Offset: 17,
		},
		types.Field{
			Name:   "tuesday",
			Type:   "bool",
			Offset: 18,
		},
		types.Field{
			Name:   "wednesday",
			Type:   "bool",
			Offset: 19,
		},
		types.Field{
			Name:   "thursday",
			Type:   "bool",
			Offset: 20,
		},
		types.Field{
			Name:   "friday",
			Type:   "bool",
			Offset: 21,
		},
		types.Field{
			Name:   "saturday",
			Type:   "bool",
			Offset: 22,
		},
		types.Field{
			Name:   "sunday",
			Type:   "bool",
			Offset: 23,
		},
		types.Field{
			Name:   "segment 1 start",
			Type:   "HHmm",
			Offset: 24,
		},
		types.Field{
			Name:   "segment 1 end",
			Type:   "HHmm",
			Offset: 26,
		},
		types.Field{
			Name:   "segment 2 start",
			Type:   "HHmm",
			Offset: 28,
		},
		types.Field{
			Name:   "segment 2 end",
			Type:   "HHmm",
			Offset: 30,
		},
		types.Field{
			Name:   "segment 3 start",
			Type:   "HHmm",
			Offset: 32,
		},
		types.Field{
			Name:   "segment 3 end",
			Type:   "HHmm",
			Offset: 34,
		},
		types.Field{
			Name:   "linked profile id",
			Type:   "uint8",
			Offset: 36,
		},
	},
}

var DeleteAllTimeProfilesRequest = types.Message{
	Name:    "delete all time profiles request",
	MsgType: 0x8a,
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

var AddTaskRequest = types.Message{
	Name:    "add task request",
	MsgType: 0xa8,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "start date",
			Type:   "date",
			Offset: 8,
		},
		types.Field{
			Name:   "end date",
			Type:   "date",
			Offset: 12,
		},
		types.Field{
			Name:   "monday",
			Type:   "bool",
			Offset: 16,
		},
		types.Field{
			Name:   "tuesday",
			Type:   "bool",
			Offset: 17,
		},
		types.Field{
			Name:   "wednesday",
			Type:   "bool",
			Offset: 18,
		},
		types.Field{
			Name:   "thursday",
			Type:   "bool",
			Offset: 19,
		},
		types.Field{
			Name:   "friday",
			Type:   "bool",
			Offset: 20,
		},
		types.Field{
			Name:   "saturday",
			Type:   "bool",
			Offset: 21,
		},
		types.Field{
			Name:   "sunday",
			Type:   "bool",
			Offset: 22,
		},
		types.Field{
			Name:   "start time",
			Type:   "HHmm",
			Offset: 23,
		},
		types.Field{
			Name:   "door",
			Type:   "uint8",
			Offset: 25,
		},
		types.Field{
			Name:   "task type",
			Type:   "uint8",
			Offset: 26,
		},
		types.Field{
			Name:   "more cards",
			Type:   "uint8",
			Offset: 27,
		},
	},
}

var RefreshTaskListRequest = types.Message{
	Name:    "refresh tasklist request",
	MsgType: 0xac,
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

var ClearTaskListRequest = types.Message{
	Name:    "clear tasklist request",
	MsgType: 0xa6,
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

var SetPCControlRequest = types.Message{
	Name:    "set pc control request",
	MsgType: 0xa0,
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
		types.Field{
			Name:   "enable",
			Type:   "bool",
			Offset: 12,
		},
	},
}

var SetInterlockRequest = types.Message{
	Name:    "set interlock request",
	MsgType: 0xa2,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "interlock",
			Type:   "uint8",
			Offset: 8,
		},
	},
}

var ActivateKeypadsRequest = types.Message{
	Name:    "activate keypads request",
	MsgType: 0xa4,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "reader 1",
			Type:   "bool",
			Offset: 8,
		},
		types.Field{
			Name:   "reader 2",
			Type:   "bool",
			Offset: 9,
		},
		types.Field{
			Name:   "reader 3",
			Type:   "bool",
			Offset: 10,
		},
		types.Field{
			Name:   "reader 4",
			Type:   "bool",
			Offset: 11,
		},
	},
}

var SetDoorPasscodesRequest = types.Message{
	Name:    "set door passcodes request",
	MsgType: 0x8c,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "door",
			Type:   "uint8",
			Offset: 8,
		},
		types.Field{
			Name:   "passcode 1",
			Type:   "uint32",
			Offset: 12,
		},
		types.Field{
			Name:   "passcode 2",
			Type:   "uint32",
			Offset: 16,
		},
		types.Field{
			Name:   "passcode 3",
			Type:   "uint32",
			Offset: 20,
		},
		types.Field{
			Name:   "passcode 4",
			Type:   "uint32",
			Offset: 24,
		},
	},
}

var GetAntiPassbackRequest = types.Message{
	Name:    "get antipassback request",
	MsgType: 0x86,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
	},
}

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
