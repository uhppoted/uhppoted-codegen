package requests

var PutCardRequest = Request{
	Message: Message{
		Name:    "put card request",
		MsgType: 0x50,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:   "card",
				Type:   "uint32",
				Offset: 8,
			},
			{
				Name:   "start date",
				Type:   "date",
				Offset: 12,
			},
			{
				Name:   "end date",
				Type:   "date",
				Offset: 16,
			},
			{
				Name:   "door 1",
				Type:   "uint8",
				Offset: 20,
			},
			{
				Name:   "door 2",
				Type:   "uint8",
				Offset: 21,
			},
			{
				Name:   "door 3",
				Type:   "uint8",
				Offset: 22,
			},
			{
				Name:   "door 4",
				Type:   "uint8",
				Offset: 23,
			},
			{
				Name:   "PIN",
				Type:   "pin",
				Offset: 24,
			},
			{
				Name:        "first card",
				Type:        "uint8",
				Tag:         "firstcard",
				Offset:      27,
				Description: "(optional) first-card privileges bitmask (bit 0: door 1, bit 1: door 2, bit 3: door 3, bit 4: door 4)",
			},
		},
	},
	Tests: []RequestTest{
		{
			Name: "put-card",
			Args: []Arg{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				{
					Name:  "card",
					Type:  "uint32",
					Value: 10058400,
				},
				{
					Name:  "start date",
					Type:  "date",
					Value: "2025-01-01",
				},
				{
					Name:  "end date",
					Type:  "date",
					Value: "2025-12-31",
				},
				{
					Name:  "door 1",
					Type:  "uint8",
					Value: 1,
				},
				{
					Name:  "door 2",
					Type:  "uint8",
					Value: 0,
				},
				{
					Name:  "door 3",
					Type:  "uint8",
					Value: 17,
				},
				{
					Name:  "door 4",
					Type:  "uint8",
					Value: 1,
				},
				{
					Name:  "PIN",
					Type:  "pin",
					Value: 999999,
				},
				{
					Name:  "first card",
					Type:  "uint8",
					Value: 11,
				},
			},
			Expected: []byte{
				0x17, 0x50, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xa0, 0x7a, 0x99, 0x00, 0x20, 0x25, 0x01, 0x01,
				0x20, 0x25, 0x12, 0x31, 0x01, 0x00, 0x11, 0x01, 0x3f, 0x42, 0x0f, 0x0b, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
	},
}
