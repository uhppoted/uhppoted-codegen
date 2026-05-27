package requests

var SetFirstCardRequest = Request{
	Message: Message{
		Name:    "set first-card request",
		MsgType: 0xaa,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:   "door",
				Type:   "uint8",
				Offset: 8,
			},
			{
				Name:   "start-time",
				Type:   "HHmm",
				Offset: 9,
			},
			{
				Name:   "end-time",
				Type:   "HHmm",
				Offset: 12,
			},
			{
				Name:   "active-mode",
				Type:   "uint8",
				Offset: 11,
			},
			{
				Name:   "inactive-mode",
				Type:   "uint8",
				Offset: 14,
			},
			{
				Name:   "monday",
				Type:   "bool",
				Offset: 15,
			},
			{
				Name:   "tuesday",
				Type:   "bool",
				Offset: 16,
			},
			{
				Name:   "wednesday",
				Type:   "bool",
				Offset: 17,
			},
			{
				Name:   "thursday",
				Type:   "bool",
				Offset: 18,
			},
			{
				Name:   "friday",
				Type:   "bool",
				Offset: 19,
			},
			{
				Name:   "saturday",
				Type:   "bool",
				Offset: 20,
			},
			{
				Name:   "sunday",
				Type:   "bool",
				Offset: 21,
			},
		},
	},

	Tests: []RequestTest{
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
			Expected: []byte{
				0x17, 0xaa, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x08, 0x30, 0x01, 0x17, 0x45, 0x02, 0x01,
				0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
	},
}
