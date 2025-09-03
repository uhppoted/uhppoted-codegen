package responses

var GetCardAtIndexResponse = Response{
	Description: []string{
		"Container struct for the response returned from a controller when retrieving",
		"the card record stored at an index on the controller.",
	},
	Message: Message{
		Name:    "get card at index response",
		MsgType: 0x5c,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:   "card",
				Type:   "uint32",
				Tag:    "card",
				Offset: 8,
			},
			{
				Name:   "start date",
				Type:   "optional date",
				Tag:    "start-date",
				Offset: 12,
			},
			{
				Name:   "end date",
				Type:   "optional date",
				Tag:    "end-date",
				Offset: 16,
			},
			{
				Name:   "door 1",
				Type:   "uint8",
				Tag:    "door-1",
				Offset: 20,
			},
			{
				Name:   "door 2",
				Type:   "uint8",
				Tag:    "door-2",
				Offset: 21,
			},
			{
				Name:   "door 3",
				Type:   "uint8",
				Tag:    "door-3",
				Offset: 22,
			},
			{
				Name:   "door 4",
				Type:   "uint8",
				Tag:    "door-4",
				Offset: 23,
			},
			{
				Name:   "PIN",
				Type:   "pin",
				Tag:    "PIN",
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
					Type:  "optional date",
					Value: "2024-01-01",
				},
				{
					Name:  "end date",
					Type:  "optional date",
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
