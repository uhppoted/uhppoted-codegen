import encode
import udp


def get_all_controllers():
    request = encode.get_controller_request(0)
    replies = udp.send(request, udp.read_all)

    # list := []*GetControllerResponse{}
    # for _, reply := range replies {
    #     if response, err := getControllerResponse(reply); err != nil {
    #         return nil, err
    #     } else if response != nil {
    #         list = append(list, response)
    #     }
    # }

    # return list, nil

    raise Exception(f'not implemented')
