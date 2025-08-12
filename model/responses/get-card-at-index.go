package responses

var GetCardAtIndexResponse = Response{
	Message: Message{
		Name:    "get card at index response",
		MsgType: 0x5c,
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
				Type:   "optional date",
				Offset: 12,
			},
			{
				Name:   "end date",
				Type:   "optional date",
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
		},
	},
	Tests: []ResponseTest{
		{
			Name: "get-card-at-index",
			Response: []byte{
				0x17, 0x5c, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xa0, 0x7a, 0x99, 0x00, 0x20, 0x24, 0x01, 0x01,
				0x20, 0x24, 0x12, 0x31, 0x01, 0x00, 0x11, 0x01, 0x3f, 0x42, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Expected: []Value{
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
					Value: "2024-01-01",
				},
				{
					Name:  "end date",
					Type:  "date",
					Value: "2024-12-31",
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
					Type:  "uint32",
					Value: 999999,
				},
			},
		},
	},
}
