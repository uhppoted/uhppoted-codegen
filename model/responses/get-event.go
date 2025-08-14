package responses

var GetEventResponse = Response{
	Message: Message{
		Name:    "get event response",
		MsgType: 0xb0,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:   "index",
				Type:   "uint32",
				Offset: 8,
			},
			{
				Name:   "event type",
				Type:   "uint8",
				Offset: 12,
			},
			{
				Name:   "access granted",
				Type:   "bool",
				Offset: 13,
			},
			{
				Name:   "door",
				Type:   "uint8",
				Offset: 14,
			},
			{
				Name:   "direction",
				Type:   "uint8",
				Offset: 15,
			},
			{
				Name:   "card",
				Type:   "uint32",
				Offset: 16,
			},
			{
				Name:   "timestamp",
				Type:   "optional datetime",
				Offset: 20,
			},
			{
				Name:   "reason",
				Type:   "uint8",
				Offset: 27,
			},
		},
	},

	Tests: []ResponseTest{
		{
			Name: "get-event",
			Response: []byte{
				0x17, 0xb0, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x0b, 0x35, 0x00, 0x00, 0x02, 0x01, 0x04, 0x02,
				0xa0, 0x7a, 0x99, 0x00, 0x20, 0x25, 0x11, 0x17, 0x12, 0x34, 0x56, 0x15, 0x00, 0x00, 0x00, 0x00,
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
					Name:  "index",
					Type:  "uint32",
					Value: 13579,
				},
				{
					Name:  "timestamp",
					Type:  "datetime",
					Value: "2025-11-17 12:34:56",
				},
				{
					Name:  "event type",
					Type:  "uint8",
					Value: 2,
				},
				{
					Name:  "access granted",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "door",
					Type:  "uint8",
					Value: 4,
				},
				{
					Name:  "direction",
					Type:  "uint8",
					Value: 2,
				},
				{
					Name:  "card",
					Type:  "uint32",
					Value: 10058400,
				},
				{
					Name:  "reason",
					Type:  "uint8",
					Value: 21,
				},
			},
		},
		{
			Name: "get-event-not-found",
			Response: []byte{
				0x17, 0xb0, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x68, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
					Name:  "index",
					Type:  "uint32",
					Value: 24680,
				},
				{
					Name:  "timestamp",
					Type:  "datetime",
					Value: "0001-01-01 00:00:00",
				},
				{
					Name:  "event type",
					Type:  "uint8",
					Value: 0x00,
				},
				{
					Name:  "access granted",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "door",
					Type:  "uint8",
					Value: 0,
				},
				{
					Name:  "direction",
					Type:  "uint8",
					Value: 0,
				},
				{
					Name:  "card",
					Type:  "uint32",
					Value: 0,
				},
				{
					Name:  "reason",
					Type:  "uint8",
					Value: 0,
				},
			},
		},
		{
			Name: "get-event-overwritten",
			Response: []byte{
				0x17, 0xb0, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xcd, 0x81, 0x01, 0x00, 0xff, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
					Name:  "index",
					Type:  "uint32",
					Value: 98765,
				},
				{
					Name:  "timestamp",
					Type:  "datetime",
					Value: "0001-01-01 00:00:00",
				},
				{
					Name:  "event type",
					Type:  "uint8",
					Value: 0xff,
				},
				{
					Name:  "access granted",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "door",
					Type:  "uint8",
					Value: 0,
				},
				{
					Name:  "direction",
					Type:  "uint8",
					Value: 0,
				},
				{
					Name:  "card",
					Type:  "uint32",
					Value: 0,
				},
				{
					Name:  "reason",
					Type:  "uint8",
					Value: 0,
				},
			},
		},
	},
}
