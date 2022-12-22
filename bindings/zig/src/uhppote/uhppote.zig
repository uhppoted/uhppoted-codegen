const std = @import("std");
const encode = @import("encode.zig");
const udp = @import("udp.zig");

pub fn set_debug(v: bool) void {
    udp.set_debug(v);
}

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
    const request = try encode.get_controller_request(405419896);

    try udp.broadcast(request);
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
