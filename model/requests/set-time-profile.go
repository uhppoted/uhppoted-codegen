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
			Args: []Arg{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				{
					Name:  "profile",
					Type:  "uint8",
					Value: 37,
				},
				{
					Name:  "start date",
					Type:  "date",
					Value: "2025-11-26",
				},
				{
					Name:  "end date",
					Type:  "date",
					Value: "2025-12-29",
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
				{
					Name:  "segment 1 start",
					Type:  "HHmm",
					Value: "8:30",
				},
				{
					Name:  "segment 1 end",
					Type:  "HHmm",
					Value: "9:45",
				},
				{
					Name:  "segment 2 start",
					Type:  "HHmm",
					Value: "11:35",
				},
				{
					Name:  "segment 2 end",
					Type:  "HHmm",
					Value: "13:15",
				},
				{
					Name:  "segment 3 start",
					Type:  "HHmm",
					Value: "14:01",
				},
				{
					Name:  "segment 3 end",
					Type:  "HHmm",
					Value: "17:59",
				},
				{
					Name:  "linked profile id",
					Type:  "uint8",
					Value: 19,
				},
			},
			Expected: []byte{
				0x17, 0x88, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x25, 0x20, 0x25, 0x11, 0x26, 0x20, 0x25, 0x12,
				0x29, 0x01, 0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x08, 0x30, 0x09, 0x45, 0x11, 0x35, 0x13, 0x15,
				0x14, 0x01, 0x17, 0x59, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
	},
}
