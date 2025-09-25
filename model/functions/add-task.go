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
			Name:        "task",
			Type:        "task",
			Description: "task type",
		},
		{
			Name:        "start date",
			Type:        "date",
			Description: "date from which task is scheduled (inclusive)",
		},
		{
			Name:        "end date",
			Type:        "date",
			Description: "date after which task no longer scheduled",
		},
		{
			Name:        "monday",
			Type:        "bool",
			Description: "task enabled on Monday if true",
		},
		{
			Name:        "tuesday",
			Type:        "bool",
			Description: "task enabled on Tuesday if true",
		},
		{
			Name:        "wednesday",
			Type:        "bool",
			Description: "task enabled on Wednesday if true",
		},
		{
			Name:        "thursday",
			Type:        "bool",
			Description: "task enabled on Thursday if true",
		},
		{
			Name:        "friday",
			Type:        "bool",
			Description: "task enabled on Monday if true",
		},
		{
			Name:        "saturday",
			Type:        "bool",
			Description: "task enabled on Friday if true",
		},
		{
			Name:        "sunday",
			Type:        "bool",
			Description: "task enabled on Sunday if true",
		},
		{
			Name:        "start time",
			Type:        "HHmm",
			Description: "time at which task is scheduled",
		},
		{
			Name:        "door",
			Type:        "uint8",
			Description: "door ([1..4] to which task is linked",
		},
		{
			Name:        "more cards",
			Type:        "uint8",
			Description: "number of 'more cards' for the MORE CARDS task type",
		},
	},
	Request:  requests.AddTaskRequest.Message,
	Response: responses.AddTaskResponse.Message,

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
						Type: "task",
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
