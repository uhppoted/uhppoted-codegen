package requests

import (
	"net/netip"
)

var SetListenerAddrPortRequest = Request{
	Message: Message{
		Name:    "set listener addr:port request",
		MsgType: 0x90,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:        "listener",
				Type:        "address:port",
				Offset:      8,
				Description: "event listener IPv4 address:port",
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
			Name: "set-listener-addrport",
			Args: []Arg{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				{
					Name:  "listener",
					Type:  "address:port",
					Value: netip.MustParseAddrPort("192.168.1.100:60001"),
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
