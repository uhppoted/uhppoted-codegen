package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var GetTimeProfile = Function{
	Name: "get time profile",
	Description: []string{
		"Retrieves the requested access time profile from a controller.",
	},
	Args: []Arg{
		{
			Name:        "controller",
			Type:        "controller",
			Description: "uint32|Controller controller serial number or {id, address, protocol} Controller struct",
		},
		{
			Name:        "profile",
			Type:        "uint8",
			Description: "profile ID ([2..254] to retrieve",
		},
	},
	Request:  requests.GetTimeProfileRequest.Message,
	Response: responses.GetTimeProfileResponse.Message,

	Tests: []FuncTest{
		{
			Name: "get-time-profile",
			Args: []TestArg{
				{
					Arg: Arg{
						Name: "controller",
						Type: "uint32",
					},
					Value: 405419896,
				},
				{
					Arg: Arg{
						Name: "profile",
						Type: "uint8",
					},
					Value: 37,
				},
			},
			Request: []byte{
				0x17, 0x98, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x25, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []TestReply{
				{
					Message: []byte{
						0x17, 0x98, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x25, 0x20, 0x25, 0x11, 0x26, 0x20, 0x25, 0x12,
						0x29, 0x01, 0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x08, 0x30, 0x09, 0x45, 0x11, 0x35, 0x13, 0x15,
						0x14, 0x01, 0x17, 0x59, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
						0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
					},
					Response: []Value{
						{
							Name:  "controller",
							Type:  "uint32",
							Value: 405419896,
						},
						{
							Name:  "profile",
							Type:  "uint8",
							Value: 37,
						},
						{
							Name:  "start date",
							Type:  "optional date",
							Value: "2025-11-26",
						},
						{
							Name:  "end date",
							Type:  "optional date",
							Value: "2025-12-29",
						},
						{
							Name:  "monday",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "tuesday",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "wednesday",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "thursday",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "friday",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "saturday",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "sunday",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "segment 1 start",
							Type:  "HHmm",
							Value: "08:30",
						},
						{
							Name:  "segment 1 end",
							Type:  "HHmm",
							Value: "09:45",
						},
						{
							Name:  "segment 2 start",
							Type:  "HHmm",
							Value: "11:35",
						},
						{
							Name:  "segment 2 end",
							Type:  "HHmm",
							Value: "13:15",
						},
						{
							Name:  "segment 3 start",
							Type:  "HHmm",
							Value: "14:01",
						},
						{
							Name:  "segment 3 end",
							Type:  "HHmm",
							Value: "17:59",
						},
						{
							Name:  "linked profile",
							Type:  "uint8",
							Value: 19,
						},
					},
				},
			},
		},
	},
}
