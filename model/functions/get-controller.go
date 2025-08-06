package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var GetController = Function{
	Name:        "get controller",
	Description: "Retrieves the system information from an access controller.",
	Args: []Arg{
		Arg{
			Name: "controller",
			Type: "controller",
		},
	},
	Request:  &requests.GetControllerRequest.Message,
	Response: &responses.GetControllerResponse.Message,
}
