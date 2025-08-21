package requests

var SetTimeProfileRequest = Request{
	Message: Message{
		Name:    "set time profile request",
		MsgType: 0x88,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:   "profile",
				Type:   "uint8",
				Offset: 8,
			},
			{
				Name:   "start date",
				Type:   "date",
				Offset: 9,
			},
			{
				Name:   "end date",
				Type:   "date",
				Offset: 13,
			},
			{
				Name:   "monday",
				Type:   "bool",
				Offset: 17,
			},
			{
				Name:   "tuesday",
				Type:   "bool",
				Offset: 18,
			},
			{
				Name:   "wednesday",
				Type:   "bool",
				Offset: 19,
			},
			{
				Name:   "thursday",
				Type:   "bool",
				Offset: 20,
			},
			{
				Name:   "friday",
				Type:   "bool",
				Offset: 21,
			},
			{
				Name:   "saturday",
				Type:   "bool",
				Offset: 22,
			},
			{
				Name:   "sunday",
				Type:   "bool",
				Offset: 23,
			},
			{
				Name:   "segment 1 start",
				Type:   "HHmm",
				Offset: 24,
			},
			{
				Name:   "segment 1 end",
				Type:   "HHmm",
				Offset: 26,
			},
			{
				Name:   "segment 2 start",
				Type:   "HHmm",
				Offset: 28,
			},
			{
				Name:   "segment 2 end",
				Type:   "HHmm",
				Offset: 30,
			},
			{
				Name:   "segment 3 start",
				Type:   "HHmm",
				Offset: 32,
			},
			{
				Name:   "segment 3 end",
				Type:   "HHmm",
				Offset: 34,
			},
			{
				Name:   "linked profile id",
				Type:   "uint8",
				Offset: 36,
			},
		},
	},

	Tests: []RequestTest{
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
					Value: "2025-11;26",
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
					Value: 405419896,
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
			Expected: []byte{
				0x17, 0x88, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x25, 0x20, 0x24, 0x11, 0x26, 0x20, 0x24, 0x12,
				0x29, 0x01, 0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x08, 0x30, 0x09, 0x45, 0x11, 0x35, 0x13, 0x15,
				0x14, 0x01, 0x17, 0x59, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
	},
}
