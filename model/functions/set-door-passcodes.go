package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var SetDoorPasscodes = Function{
	Name: "set door passcodes",
	Description: []string{
		"Sets up to 4 passcodes for a controller door.",
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
			Name:        "passcode 1",
			Type:        "pin",
			Description: "supervisor passcode ([0..99999]), 0 for 'none'",
		},
		{
			Name:        "passcode 2",
			Type:        "pin",
			Description: "supervisor passcode ([0..99999]), 0 for 'none'",
		},
		{
			Name:        "passcode 3",
			Type:        "pin",
			Description: "supervisor passcode ([0..99999]), 0 for 'none'",
		},
		{
			Name:        "passcode 4",
			Type:        "pin",
			Description: "supervisor passcode ([0..99999]), 0 for 'none'",
		},
	},
	Request:   requests.SetDoorPasscodesRequest.Message,
	Response:  responses.SetDoorPasscodesResponse.Message,
	Protocols: []string{"broadcast", "udp", "tcp"},

	Tests: []FuncTest{
		{
			Name: "set-door-passcodes",
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
						Name: "passcode 1",
						Type: "uint32",
					},
					Value: 12345,
				},
				{
					Arg: Arg{
						Name: "passcode 2",
						Type: "uint32",
					},
					Value: 54321,
				},
				{
					Arg: Arg{
						Name: "passcode 3",
						Type: "uint32",
					},
					Value: 999999,
				},
				{
					Arg: Arg{
						Name: "passcode 4",
						Type: "uint32",
					},
					Value: 0,
				},
			},
			Request: []byte{
				0x17, 0x8c, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x04, 0x00, 0x00, 0x00, 0x39, 0x30, 0x00, 0x00,
				0x31, 0xd4, 0x00, 0x00, 0x3f, 0x42, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []TestReply{
				{
					Message: []byte{
						0x17, 0x8c, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
							Type:  "bool",
							Value: true,
						},
					},
				},
			},
		},
	},
}
