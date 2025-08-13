package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var PutCard = Function{
	Name:        "put card",
	Description: "Creates or updates a card record stored on an access controller.",
	Args: []Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "card number",
			Type: "uint32",
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
			Name: "door 1",
			Type: "uint8",
		},
		{
			Name: "door 2",
			Type: "uint8",
		},
		{
			Name: "door 3",
			Type: "uint8",
		},
		{
			Name: "door 4",
			Type: "uint8",
		},
		{
			Name: "PIN",
			Type: "pin",
		},
	},
	Request:  &requests.PutCardRequest.Message,
	Response: &responses.PutCardResponse.Message,

	Tests: []FuncTest{
		{
			Name: "put-card",
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
						Name: "card",
						Type: "uint32",
					},
					Value: 10058400,
				},
				{
					Arg: Arg{
						Name: "start date",
						Type: "date",
					},
					Value: "2025-01-01",
				},
				{
					Arg: Arg{
						Name: "end date",
						Type: "date",
					},
					Value: "2025-12-31",
				},
				{
					Arg: Arg{
						Name: "door 1",
						Type: "uint8",
					},
					Value: 1,
				},
				{
					Arg: Arg{
						Name: "door 2",
						Type: "uint8",
					},
					Value: 0,
				},
				{
					Arg: Arg{
						Name: "door 3",
						Type: "uint8",
					},
					Value: 17,
				},
				{
					Arg: Arg{
						Name: "door 4",
						Type: "uint8",
					},
					Value: 1,
				},
				{
					Arg: Arg{
						Name: "PIN",
						Type: "pin",
					},
					Value: 999999,
				},
			},
			Request: []byte{
				0x17, 0x50, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xa0, 0x7a, 0x99, 0x00, 0x20, 0x25, 0x01, 0x01,
				0x20, 0x25, 0x12, 0x31, 0x01, 0x00, 0x11, 0x01, 0x3f, 0x42, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []TestReply{
				{
					Message: []byte{
						0x17, 0x50, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
