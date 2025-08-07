package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var PutCard = Function{
	Name: "put card",
	Args: []Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "card number",
			Type: "uint32",
		},
		{
			Name: "start date",
			Type: "date",
		},
		{
			Name: "end date",
			Type: "date",
		},
		{
			Name: "door 1",
			Type: "uint8",
		},
		{
			Name: "door 2",
			Type: "uint8",
		},
		{
			Name: "door 3",
			Type: "uint8",
		},
		{
			Name: "door 4",
			Type: "uint8",
		},
		{
			Name: "PIN",
			Type: "pin",
		},
	},
	Request:  &requests.PutCardRequest.Message,
	Response: &responses.PutCardResponse.Message,
}
