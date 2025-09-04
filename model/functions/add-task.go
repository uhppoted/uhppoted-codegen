package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var AddTask = Function{
	Name: "add task",
	Description: []string{
		"Creates a scheduled task.",
		"",
		"Task types",
		"0:  control door",
		"1:  unlock door",
		"2:  lock door",
		"3:  disable time profiles",
		"4:  enable time profiles",
		"5:  enable card, no password",
		"6:  enable card+IN password",
		"7:  enable card+password",
		"8:  enable more cards",
		"9:  disable more cards",
		"10: trigger once",
		"11: disable pushbutton",
		"12: enable pushbutton",
	},
	Args: []Arg{
		{
			Name:        "controller",
			Type:        "controller",
			Description: "uint32|Controller controller serial number or {id, address, protocol} Controller struct",
		},
		{
			Name: "task",
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
			Name: "start time",
			Type: "HHmm",
		},
		{
			Name: "door",
			Type: "uint8",
		},
		{
			Name: "more cards",
			Type: "uint8",
		},
	},
	Request:   requests.AddTaskRequest.Message,
	Response:  responses.AddTaskResponse.Message,
	Protocols: []string{"broadcast", "udp", "tcp"},

	Tests: []FuncTest{
		{
			Name: "add-task",
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
						Name: "task",
						Type: "uint8",
					},
					Value: "2",
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
						Name: "monday",
						Type: "bool",
					},
					Value: "true",
				},
				{
					Arg: Arg{
						Name: "tuesday",
						Type: "bool",
					},
					Value: "true",
				},
				{
					Arg: Arg{
						Name: "wednesday",
						Type: "bool",
					},
					Value: "false",
				},
				{
					Arg: Arg{
						Name: "thursday",
						Type: "bool",
					},
					Value: "true",
				},
				{
					Arg: Arg{
						Name: "friday",
						Type: "bool",
					},
					Value: "false",
				},
				{
					Arg: Arg{
						Name: "saturday",
						Type: "bool",
					},
					Value: "true",
				},
				{
					Arg: Arg{
						Name: "sunday",
						Type: "bool",
					},
					Value: "true",
				},
				{
					Arg: Arg{
						Name: "start time",
						Type: "HHmm",
					},
					Value: "08:45",
				},
				{
					Arg: Arg{
						Name: "door",
						Type: "uint8",
					},
					Value: "3",
				},
				{
					Arg: Arg{
						Name: "more cards",
						Type: "uint8",
					},
					Value: "7",
				},
			},
			Request: []byte{
				0x17, 0xa8, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x20, 0x25, 0x01, 0x01, 0x20, 0x25, 0x12, 0x31,
				0x01, 0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x08, 0x45, 0x03, 0x02, 0x07, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []TestReply{
				{
					Message: []byte{
						0x17, 0xa8, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
