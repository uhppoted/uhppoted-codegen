package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var SetDoor = Function{
	Name: "set door",
	Description: []string{
		"Sets the control mode and unlock delay time for an access controller door.",
	},
	Args: []Arg{
		{
			Name:        "controller",
			Type:        "controller",
			Description: "uint32|Controller controller serial number or {id, address, protocol} Controller struct",
		},
		{
			Name:        "door",
			Type:        "uint8",
			Description: "door ID ([1..4])",
		},
		{
			Name:        "mode",
			Type:        "uint8",
			Description: "control mode (1:normally open, 2:normally closed. 3:controlled)",
		},
		{
			Name:        "delay",
			Type:        "uint8",
			Description: "unlock delay (seconds))",
		},
	},
	Request:  requests.SetDoorRequest.Message,
	Response: responses.SetDoorResponse.Message,

	Tests: []FuncTest{
		{
			Name: "set-door",
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
						Name: "door",
						Type: "uint8",
					},
					Value: 4,
				},
				{
					Arg: Arg{
						Name: "mode",
						Type: "uint8",
					},
					Value: 2,
				},
				{
					Arg: Arg{
						Name: "delay",
						Type: "uint8",
					},
					Value: 17,
				},
			},
			Request: []byte{
				0x17, 0x80, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x04, 0x02, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []TestReply{
				{
					Message: []byte{
						0x17, 0x80, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x04, 0x02, 0x11, 0x00, 0x00, 0x00, 0x00, 0x00,
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
							Name:  "door",
							Type:  "uint8",
							Value: 4,
						},
						{
							Name:  "mode",
							Type:  "uint8",
							Value: 2,
						},
						{
							Name:  "delay",
							Type:  "uint8",
							Value: 17,
						},
					},
				},
			},
		},
	},
}
