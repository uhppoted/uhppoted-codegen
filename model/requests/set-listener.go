package requests

import (
	"net/netip"
)

var SetListenerRequest = Request{
	Message: Message{
		Name:    "set listener request",
		MsgType: 0x90,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:        "address",
				Type:        "IPv4",
				Offset:      8,
				Description: "event listener IPv4 address",
			},
			{
				Name:        "port",
				Type:        "uint16",
				Offset:      12,
				Description: "event listener IPv4 port",
			},
			{
				Name:        "interval",
				Type:        "uint8",
				Offset:      14,
				Description: "status auto-send interval (seconds)",
			},
		},
	},
	Tests: []RequestTest{
		{
			Name: "set-listener",
			Args: []Arg{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				{
					Name:  "address",
					Type:  "IPv4",
					Value: netip.MustParseAddr("192.168.1.100"),
				},
				{
					Name:  "port",
					Type:  "uint16",
					Value: 60001,
				},
				{
					Name:  "interval",
					Type:  "uint8",
					Value: 17,
				},
			},
			Expected: []byte{
				0x17, 0x90, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xc0, 0xa8, 0x01, 0x64, 0x61, 0xea, 0x11, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
	},
}
