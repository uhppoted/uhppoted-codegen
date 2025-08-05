package responses

var GetCardResponse = Response{
	Message: Message{
		Name:    "get card response",
		MsgType: 0x5a,
		Fields: []Field{
			Field{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			Field{
				Name:   "card number",
				Type:   "uint32",
				Offset: 8,
			},
			Field{
				Name:   "start date",
				Type:   "optional date",
				Offset: 12,
			},
			Field{
				Name:   "end date",
				Type:   "optional date",
				Offset: 16,
			},
			Field{
				Name:   "door 1",
				Type:   "uint8",
				Offset: 20,
			},
			Field{
				Name:   "door 2",
				Type:   "uint8",
				Offset: 21,
			},
			Field{
				Name:   "door 3",
				Type:   "uint8",
				Offset: 22,
			},
			Field{
				Name:   "door 4",
				Type:   "uint8",
				Offset: 23,
			},
			Field{
				Name:   "PIN",
				Type:   "pin",
				Offset: 24,
			},
		},
	},
	Tests: []ResponseTest{
		{
			Name: "get-card",
			Response: []byte{
				0x17, 0x5a, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xa0, 0x7a, 0x99, 0x00, 0x20, 0x24, 0x01, 0x01,
				0x20, 0x24, 0x12, 0x31, 0x01, 0x00, 0x11, 0x01, 0x3f, 0x42, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Expected: []Value{
				Value{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				Value{
					Name:  "card",
					Type:  "uint32",
					Value: 10058400,
				},
				Value{
					Name:  "start date",
					Type:  "date",
					Value: "2024-01-01",
				},
				Value{
					Name:  "end date",
					Type:  "date",
					Value: "2024-12-31",
				},
				Value{
					Name:  "door 1",
					Type:  "uint8",
					Value: 1,
				},
				Value{
					Name:  "door 2",
					Type:  "uint8",
					Value: 0,
				},
				Value{
					Name:  "door 3",
					Type:  "uint8",
					Value: 17,
				},
				Value{
					Name:  "door 4",
					Type:  "uint8",
					Value: 1,
				},
				Value{
					Name:  "PIN",
					Type:  "uint32",
					Value: 999999,
				},
			},
		},
	},
}
