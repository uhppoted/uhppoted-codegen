package model

import (
	"github.com/uhppoted/uhppoted-codegen/model/responses"
	"github.com/uhppoted/uhppoted-codegen/model/types"
)

var Responses = []types.Message{
	GetControllerResponse.Message,
	SetIPv4Response.Message,
	GetTimeResponse.Message,
	SetTimeResponse.Message,
	GetListenerResponse.Message,
	SetListenerResponse.Message,
	GetDoorResponse.Message,
	SetDoorResponse.Message,
	SetDoorPasscodesResponse.Message,
	OpenDoorResponse.Message,
	GetStatusResponse.Message,
	GetCardsResponse.Message,
	GetCardResponse.Message,
	GetCardAtIndexResponse.Message,
	PutCardResponse.Message,
	DeleteCardResponse.Message,
	DeleteAllCardsResponse.Message,
	GetEventResponse.Message,
	GetEventIndexResponse.Message,
	SetEventIndexResponse.Message,
	RecordSpecialEventsResponse,
	GetTimeProfileResponse,
	SetTimeProfileResponse,
	DeleteAllTimeProfilesResponse,
	AddTaskResponse,
	RefreshTaskListResponse,
	ClearTaskListResponse,
	SetPCControlResponse,
	SetInterlockResponse,
	ActivateKeypadsResponse,
	GetAntiPassbackResponse,
	SetAntiPassbackResponse,
	RestoreDefaultParametersResponse,
}

var GetControllerResponse = responses.GetControllerResponse
var SetIPv4Response = responses.SetIPv4Response
var GetTimeResponse = responses.GetTimeResponse
var SetTimeResponse = responses.SetTimeResponse
var GetListenerResponse = responses.GetListenerResponse
var GetListenerAddrPortResponse = responses.GetListenerAddrPortResponse
var SetListenerResponse = responses.SetListenerResponse
var GetDoorResponse = responses.GetDoorResponse
var SetDoorResponse = responses.SetDoorResponse
var SetDoorPasscodesResponse = responses.SetDoorPasscodesResponse
var OpenDoorResponse = responses.OpenDoorResponse
var GetStatusResponse = responses.GetStatusResponse
var GetCardsResponse = responses.GetCardsResponse
var GetCardResponse = responses.GetCardResponse
var GetCardAtIndexResponse = responses.GetCardAtIndexResponse
var PutCardResponse = responses.PutCardResponse
var DeleteCardResponse = responses.DeleteCardResponse
var DeleteAllCardsResponse = responses.DeleteAllCardsResponse
var GetEventResponse = responses.GetEventResponse
var GetEventIndexResponse = responses.GetEventIndexResponse
var SetEventIndexResponse = responses.SetEventIndexResponse

// var SetEventIndexResponse = types.Message{
// 	Name:    "set event index response",
// 	MsgType: 0xb2,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "updated",
// 			Type:   "bool",
// 			Offset: 8,
// 		},
// 	},
// }

var RecordSpecialEventsResponse = types.Message{
	Name:    "record special events response",
	MsgType: 0x8e,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "updated",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var GetTimeProfileResponse = types.Message{
	Name:    "get time profile response",
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
		types.Field{
			Name:   "start date",
			Type:   "optional date",
			Offset: 9,
		},
		types.Field{
			Name:   "end date",
			Type:   "optional date",
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

var SetTimeProfileResponse = types.Message{
	Name:    "set time profile response",
	MsgType: 0x88,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "stored",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var DeleteAllTimeProfilesResponse = types.Message{
	Name:    "delete all time profiles response",
	MsgType: 0x8a,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "deleted",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var AddTaskResponse = types.Message{
	Name:    "add task response",
	MsgType: 0xa8,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "added",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var RefreshTaskListResponse = types.Message{
	Name:    "refresh tasklist response",
	MsgType: 0xac,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "refreshed",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var ClearTaskListResponse = types.Message{
	Name:    "clear tasklist response",
	MsgType: 0xa6,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "cleared",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var SetPCControlResponse = types.Message{
	Name:    "set pc control response",
	MsgType: 0xa0,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var SetInterlockResponse = types.Message{
	Name:    "set interlock response",
	MsgType: 0xa2,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var ActivateKeypadsResponse = types.Message{
	Name:    "activate keypads response",
	MsgType: 0xa4,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

// var SetDoorPasscodesResponse = types.Message{
// 	Name:    "set door passcodes response",
// 	MsgType: 0x8c,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "ok",
// 			Type:   "bool",
// 			Offset: 8,
// 		},
// 	},
// }

var GetAntiPassbackResponse = types.Message{
	Name:    "get antipassback response",
	MsgType: 0x86,
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

var SetAntiPassbackResponse = types.Message{
	Name:    "set antipassback response",
	MsgType: 0x84,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "ok",
			Type:   "bool",
			Offset: 8,
		},
	},
}

var RestoreDefaultParametersResponse = types.Message{
	Name:    "restore default parameters response",
	MsgType: 0xc8,
	Fields: []types.Field{
		types.Field{
			Name:        "controller",
			Type:        "uint32",
			Offset:      4,
			Description: "controller serial number",
		},
		types.Field{
			Name:   "reset",
			Type:   "bool",
			Offset: 8,
		},
	},
}
