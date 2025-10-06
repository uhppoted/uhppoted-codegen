package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var GetStatus = Function{
	Name:        "get status",
	Description: []string{"Retrieves the system status from an access controller."},
	Args: []Arg{
		{
			Name:        "controller",
			Type:        "controller",
			Description: "uint32|Controller controller serial number or {id, address, protocol} Controller struct",
		},
	},
	Request:  requests.GetStatusRequest.Message,
	Response: responses.GetStatusResponse.Message,

	Tests: []FuncTest{
		{
			Name: "get-status",
			Args: []Arg{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
			},
			Request: []byte{
				0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []Reply{
				{
					Message: []byte{
						0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
						0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
						0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
						0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
					},
					Response: []Value{
						{
							Name:  "controller",
							Type:  "uint32",
							Value: 405419896,
						},
						{
							Name:  "system-date",
							Type:  "shortdate",
							Value: "2022-08-23",
						},
						{
							Name:  "system-time",
							Type:  "time",
							Value: "09:49:39",
						},
						{
							Name:  "door-1-open",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "door-2-open",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "door-3-open",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "door-4-open",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "door-1-button",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "door-2-button",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "door-3-button",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "door-4-button",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "relays",
							Type:  "uint8",
							Value: 0x07,
						},
						{
							Name:  "inputs",
							Type:  "uint8",
							Value: 0x09,
						},
						{
							Name:  "system-error",
							Type:  "uint8",
							Value: 3,
						},
						{
							Name:  "special-info",
							Type:  "uint8",
							Value: 39,
						},
						{
							Name:  "event-index",
							Type:  "uint32",
							Value: 78,
						},
						{
							Name:  "event-type",
							Type:  "uint8",
							Value: 2,
						},
						{
							Name:  "event-access-granted",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "event-door",
							Type:  "uint8",
							Value: 3,
						},
						{
							Name:  "event-direction",
							Type:  "uint8",
							Value: 1,
						},
						{
							Name:  "event-card",
							Type:  "uint32",
							Value: 8165537,
						},
						{
							Name:  "event-timestamp",
							Type:  "optional datetime",
							Value: "2022-08-23 09:47:06",
						},
						{
							Name:  "event-reason",
							Type:  "uint8",
							Value: 44,
						},
						{
							Name:  "sequence-no",
							Type:  "uint32",
							Value: 0,
						},
					},
				},
			},
		},
		{
			Name: "get-status-no-event",
			Args: []Arg{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419897,
				},
			},
			Request: []byte{
				0x17, 0x20, 0x00, 0x00, 0x79, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []Reply{
				{
					Message: []byte{
						0x17, 0x20, 0x00, 0x00, 0x79, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
						0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x01,
						0x01, 0x01, 0x00, 0x01, 0x1b, 0x14, 0x37, 0x53, 0xe3, 0x55, 0x00, 0x00, 0x21, 0x00, 0x00, 0x00,
						0x27, 0x07, 0x09, 0x25, 0x11, 0x23, 0x00, 0x00, 0x93, 0x26, 0x04, 0x88, 0x08, 0x92, 0x00, 0x00,
					},
					Response: []Value{
						{
							Name:  "controller",
							Type:  "uint32",
							Value: 405419897,
						},
						{
							Name:  "system-date",
							Type:  "shortdate",
							Value: "2025-11-23",
						},
						{
							Name:  "system-time",
							Type:  "time",
							Value: "14:37:53",
						},
						{
							Name:  "door-1-open",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "door-2-open",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "door-3-open",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "door-4-open",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "door-1-button",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "door-2-button",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "door-3-button",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "door-4-button",
							Type:  "bool",
							Value: true,
						},
						{
							Name:  "relays",
							Type:  "uint8",
							Value: 0x07,
						},
						{
							Name:  "inputs",
							Type:  "uint8",
							Value: 0x09,
						},
						{
							Name:  "system-error",
							Type:  "uint8",
							Value: 27,
						},
						{
							Name:  "special-info",
							Type:  "uint8",
							Value: 39,
						},
						{
							Name:  "event-index",
							Type:  "uint32",
							Value: 0,
						},
						{
							Name:  "event-type",
							Type:  "uint8",
							Value: 0,
						},
						{
							Name:  "event-access-granted",
							Type:  "bool",
							Value: false,
						},
						{
							Name:  "event-door",
							Type:  "uint8",
							Value: 0,
						},
						{
							Name:  "event-direction",
							Type:  "uint8",
							Value: 0,
						},
						{
							Name:  "event-card",
							Type:  "uint32",
							Value: 0,
						},
						{
							Name:  "event-timestamp",
							Type:  "optional datetime",
							Value: "0001-01-01 00:00:00",
						},
						{
							Name:  "event-reason",
							Type:  "uint8",
							Value: 0,
						},
						{
							Name:  "sequence-no",
							Type:  "uint32",
							Value: 21987,
						},
					},
				},
			},
		},
	},
}
