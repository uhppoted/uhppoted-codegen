const std = @import("std");
const encode = @import("encode.zig");

pub const GetControllerResponse = struct {
    controller: u32,
    //    pub ip_address: Ipv4Addr,
    //    pub subnet_mask: Ipv4Addr,
    //    pub gateway: Ipv4Addr,
    //    pub mac_address: String,
    //    pub version: String,
    //    pub date: NaiveDate,
};

pub fn get_all_controllers() ![]GetControllerResponse {
    const request = try encode.get_controller_request(0);

    broadcast(request);
    //    replies, err := broadcast(request)
    //    if err != nil {
    //        return nil, err
    //    }
    //
    //    list := []*GetControllerResponse{}
    //    for _, reply := range replies {
    //        if response, err := getControllerResponse(reply); err != nil {
    //            return nil, err
    //        } else if response != nil {
    //            list = append(list, response)
    //        }
    //    }
    //
    //    return list, nil

    const controller = GetControllerResponse{
        .controller = 405419896,
    };

    var list = [_]GetControllerResponse{
        controller,
    };

    return &list;
}

fn broadcast(_: [64]u8) void {}
