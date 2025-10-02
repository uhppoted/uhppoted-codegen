package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var ActivateKeypads = Function{
	Name: "activate keypads",
	Description: []string{
		"Enables/disables door keypad readers.",
	},
	Args: []Arg{
		{
			Name:        "controller",
			Type:        "controller",
			Description: "uint32|Controller controller serial number or {id, address, protocol} Controller struct",
		},
		{
			Name:        "reader 1",
			Type:        "bool",
			Description: "enables/disable the keypad for reader 1",
		},
		{
			Name:        "reader 2",
			Type:        "bool",
			Description: "enables/disables the keypad for reader 2",
		},
		{
			Name:        "reader 3",
			Type:        "bool",
			Description: "enables/disables the keypad for reader 3",
		},
		{
			Name:        "reader 4",
			Type:        "bool",
			Description: "enables/disables the keypad for reader 4",
		},
	},
	Request:  requests.ActivateKeypadsRequest.Message,
	Response: responses.ActivateKeypadsResponse.Message,

	Tests: []FuncTest{
		{
			Name: "activate-keypads",
			Args: []Arg{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				{
					Name:  "reader 1",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "reader 2",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "reader 3",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "reader 4",
					Type:  "bool",
					Value: true,
				},
			},
			Request: []byte{
				0x17, 0xa4, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []TestReply{
				{
					Message: []byte{
						0x17, 0xa4, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
