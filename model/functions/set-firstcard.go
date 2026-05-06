package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var SetFirstCard = Function{
	Description: []string{
		"Sets the first card configuration for a access controller managed door.",
	},

	Name: "set first-card",
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
			Name:        "start-time",
			Type:        "HHmm",
			Description: "time from which first card can be activated",
		},
		{
			Name:        "end-time",
			Type:        "HHmm",
			Description: "time after which first card is deactivated",
		},
		{
			Name:        "active-mode",
			Type:        "uint8",
			Description: "control mode (0:normally open, 1:normally closed, 2:controlled)",
		},
		{
			Name:        "inactive-mode",
			Type:        "uint8",
			Description: "control mode (0:normally open, 1:normally closed, 2:controlled, 3:first-card only)",
		},
		{
			Name:        "monday",
			Type:        "bool",
			Description: "frist-card enabled on Monday if true",
		},
		{
			Name:        "tuesday",
			Type:        "bool",
			Description: "frist-card enabled on Tuesday if true",
		},
		{
			Name:        "wednesday",
			Type:        "bool",
			Description: "frist-card enabled on Wednesday if true",
		},
		{
			Name:        "thursday",
			Type:        "bool",
			Description: "frist-card enabled on Thursday if true",
		},
		{
			Name:        "friday",
			Type:        "bool",
			Description: "frist-card enabled on Friday if true",
		},
		{
			Name:        "saturday",
			Type:        "bool",
			Description: "frist-card enabled on Saturday if true",
		},
		{
			Name:        "sunday",
			Type:        "bool",
			Description: "frist-card enabled on Sunday if true",
		},
	},
	Request:  requests.SetFirstCardRequest.Message,
	Response: responses.SetFirstCardResponse.Message,

	Tests: []FuncTest{
		{
			Name: "set-first-card",
			Args: []Arg{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				{
					Name:  "door",
					Type:  "uint8",
					Value: 3,
				},
				{
					Name:  "start-time",
					Type:  "HHmm",
					Value: "8:30",
				},
				{
					Name:  "end-time",
					Type:  "HHmm",
					Value: "17:45",
				},
				{
					Name:  "active-mode",
					Type:  "uint8",
					Value: 1,
				},
				{
					Name:  "inactive-mode",
					Type:  "uint8",
					Value: 2,
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
			},
			Request: []byte{
				0x17, 0xaa, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x08, 0x30, 0x01, 0x17, 0x45, 0x02, 0x01,
				0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []Reply{
				{
					Message: []byte{
						0x17, 0xaa, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
