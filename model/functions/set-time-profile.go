package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var SetTimeProfile = Function{
	Name: "set time profile",
	Description: []string{
		"Adds or updates an access time profile stored on a controller.",
	},
	Args: []Arg{
		{
			Name:        "controller",
			Type:        "controller",
			Description: "uint32|Controller controller serial number or {id, address, protocol} Controller struct",
		},
		{
			Name: "profile",
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
	Request:   requests.SetTimeProfileRequest.Message,
	Response:  responses.SetTimeProfileResponse.Message,
	Protocols: []string{"broadcast", "udp", "tcp"},

	Tests: []FuncTest{
		{
			Name: "set-time-profile",
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
					}, Value: 37,
				},
				{
					Arg: Arg{
						Name: "start date",
						Type: "date",
					},
					Value: "2025-11-26",
				},
				{
					Arg: Arg{
						Name: "end date",
						Type: "date",
					},
					Value: "2025-12-29",
				},
				{
					Arg: Arg{
						Name: "monday",
						Type: "bool",
					},
					Value: true,
				},
				{
					Arg: Arg{
						Name: "tuesday",
						Type: "bool",
					},
					Value: true,
				},
				{
					Arg: Arg{
						Name: "wednesday",
						Type: "bool",
					},
					Value: false,
				},
				{
					Arg: Arg{
						Name: "thursday",
						Type: "bool",
					},
					Value: true,
				},
				{
					Arg: Arg{
						Name: "friday",
						Type: "bool",
					},
					Value: false,
				},
				{
					Arg: Arg{
						Name: "saturday",
						Type: "bool",
					},
					Value: true,
				},
				{
					Arg: Arg{
						Name: "sunday",
						Type: "bool",
					},
					Value: true,
				},
				{
					Arg: Arg{
						Name: "segment 1 start",
						Type: "HHmm",
					},
					Value: "8:30",
				},
				{
					Arg: Arg{
						Name: "segment 1 end",
						Type: "HHmm",
					},
					Value: "9:45",
				},
				{
					Arg: Arg{
						Name: "segment 2 start",
						Type: "HHmm",
					},
					Value: "11:35",
				},
				{
					Arg: Arg{
						Name: "segment 2 end",
						Type: "HHmm",
					},
					Value: "13:15",
				},
				{
					Arg: Arg{
						Name: "segment 3 start",
						Type: "HHmm",
					},
					Value: "14:01",
				},
				{
					Arg: Arg{
						Name: "segment 3 end",
						Type: "HHmm",
					},
					Value: "17:59",
				},
				{
					Arg: Arg{
						Name: "linked profile id",
						Type: "uint8",
					},
					Value: 19,
				},
			},
			Request: []byte{
				0x17, 0x88, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x25, 0x20, 0x25, 0x11, 0x26, 0x20, 0x25, 0x12,
				0x29, 0x01, 0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x08, 0x30, 0x09, 0x45, 0x11, 0x35, 0x13, 0x15,
				0x14, 0x01, 0x17, 0x59, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []TestReply{
				{
					Message: []byte{
						0x17, 0x88, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
						0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
						0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
						0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
					},
					Response: []Value{
						{
							Name:  "controller",
							Type:  "uint32",
							Value: 405419896,
						},
						{
							Name:  "ok",
							Type:  "uint8",
							Value: true,
						},
					},
				},
			},
		},
	},
}
