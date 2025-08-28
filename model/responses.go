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
	// GetListenerAddrPortResponse.Message,
	// SetListenerAddrPortResponse.Message,
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
	RecordSpecialEventsResponse.Message,
	GetTimeProfileResponse.Message,
	SetTimeProfileResponse.Message,
	ClearTimeProfilesResponse.Message,
	AddTaskResponse.Message,
	RefreshTaskListResponse.Message,
	ClearTaskListResponse.Message,
	SetPCControlResponse.Message,
	SetInterlockResponse.Message,
	ActivateKeypadsResponse.Message,
	GetAntiPassbackResponse.Message,
	SetAntiPassbackResponse.Message,
	RestoreDefaultParametersResponse.Message,
}

var GetControllerResponse = responses.GetControllerResponse
var SetIPv4Response = responses.SetIPv4Response
var GetTimeResponse = responses.GetTimeResponse
var SetTimeResponse = responses.SetTimeResponse
var GetListenerResponse = responses.GetListenerResponse
var SetListenerResponse = responses.SetListenerResponse
var GetListenerAddrPortResponse = responses.GetListenerAddrPortResponse
var SetListenerAddrPortResponse = responses.SetListenerAddrPortResponse
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
var RecordSpecialEventsResponse = responses.RecordSpecialEventsResponse
var GetTimeProfileResponse = responses.GetTimeProfileResponse
var SetTimeProfileResponse = responses.SetTimeProfileResponse
var ClearTimeProfilesResponse = responses.ClearTimeProfilesResponse
var AddTaskResponse = responses.AddTaskResponse
var RefreshTaskListResponse = responses.RefreshTaskListResponse
var ClearTaskListResponse = responses.ClearTaskListResponse
var SetPCControlResponse = responses.SetPCControlResponse
var SetInterlockResponse = responses.SetInterlockResponse
var ActivateKeypadsResponse = responses.ActivateKeypadsResponse
var GetAntiPassbackResponse = responses.GetAntiPassbackResponse
var SetAntiPassbackResponse = responses.SetAntiPassbackResponse
var RestoreDefaultParametersResponse = responses.RestoreDefaultParametersResponse

// var AddTaskResponse = types.Message{
// 	Name:    "add task response",
// 	MsgType: 0xa8,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "added",
// 			Type:   "bool",
// 			Offset: 8,
// 		},
// 	},
// }

// var RefreshTaskListResponse = types.Message{
// 	Name:    "refresh tasklist response",
// 	MsgType: 0xac,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "refreshed",
// 			Type:   "bool",
// 			Offset: 8,
// 		},
// 	},
// }

// var ClearTaskListResponse = types.Message{
// 	Name:    "clear tasklist response",
// 	MsgType: 0xa6,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "cleared",
// 			Type:   "bool",
// 			Offset: 8,
// 		},
// 	},
// }

// var SetPCControlResponse = types.Message{
// 	Name:    "set pc control response",
// 	MsgType: 0xa0,
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

// var SetInterlockResponse = types.Message{
// 	Name:    "set interlock response",
// 	MsgType: 0xa2,
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

// var ActivateKeypadsResponse = types.Message{
// 	Name:    "activate keypads response",
// 	MsgType: 0xa4,
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

// var GetAntiPassbackResponse = types.Message{
// 	Name:    "get antipassback response",
// 	MsgType: 0x86,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "antipassback",
// 			Type:   "uint8",
// 			Offset: 8,
// 		},
// 	},
// }

// var SetAntiPassbackResponse = types.Message{
// 	Name:    "set antipassback response",
// 	MsgType: 0x84,
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

// var RestoreDefaultParametersResponse = types.Message{
// 	Name:    "restore default parameters response",
// 	MsgType: 0xc8,
// 	Fields: []types.Field{
// 		types.Field{
// 			Name:        "controller",
// 			Type:        "uint32",
// 			Offset:      4,
// 			Description: "controller serial number",
// 		},
// 		types.Field{
// 			Name:   "reset",
// 			Type:   "bool",
// 			Offset: 8,
// 		},
// 	},
// }
