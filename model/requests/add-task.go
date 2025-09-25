package requests

var AddTaskRequest = Request{
	Message: Message{
		Name:    "add task request",
		MsgType: 0xa8,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:   "task",
				Type:   "task",
				Offset: 26,
			},
			{
				Name:   "start date",
				Type:   "date",
				Offset: 8,
			},
			{
				Name:   "end date",
				Type:   "date",
				Offset: 12,
			},
			{
				Name:   "monday",
				Type:   "bool",
				Offset: 16,
			},
			{
				Name:   "tuesday",
				Type:   "bool",
				Offset: 17,
			},
			{
				Name:   "wednesday",
				Type:   "bool",
				Offset: 18,
			},
			{
				Name:   "thursday",
				Type:   "bool",
				Offset: 19,
			},
			{
				Name:   "friday",
				Type:   "bool",
				Offset: 20,
			},
			{
				Name:   "saturday",
				Type:   "bool",
				Offset: 21,
			},
			{
				Name:   "sunday",
				Type:   "bool",
				Offset: 22,
			},
			{
				Name:   "start time",
				Type:   "HHmm",
				Offset: 23,
			},
			{
				Name:   "door",
				Type:   "uint8",
				Offset: 25,
			},
			{
				Name:   "more cards",
				Type:   "uint8",
				Offset: 27,
			},
		},
	},
	Tests: []RequestTest{
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
			Expected: []byte{
				0x17, 0xa8, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x20, 0x25, 0x01, 0x01, 0x20, 0x25, 0x12, 0x31,
				0x01, 0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x08, 0x45, 0x03, 0x02, 0x07, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
	},
}
