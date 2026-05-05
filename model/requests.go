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
	SetAntiPassbackRequest.Message,
	SetFirstCardRequest.Message,
	RestoreDefaultParametersRequest.Message,
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
var SetAntiPassbackRequest = requests.SetAntiPassbackRequest
var SetFirstCardRequest = requests.SetFirstCardRequest
var RestoreDefaultParametersRequest = requests.RestoreDefaultParametersRequest
