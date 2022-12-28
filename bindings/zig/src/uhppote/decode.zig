const std = @import("std");

const UhppotedError = error{ InvalidReplyStartOfMessage, InvalidReplyFunctionCode };

pub const GetControllerResponse = struct {
    controller: u32,
    //    pub ip_address: Ipv4Addr,
    //    pub subnet_mask: Ipv4Addr,
    //    pub gateway: Ipv4Addr,
    //    pub mac_address: String,
    //    pub version: String,
    //    pub date: NaiveDate,
};

pub fn get_controller_response(packet: [64]u8) !GetControllerResponse {
    // Ref. v6.62 firmware event
    if ((packet[0] != 0x17) and (packet[0] != 0x19 or packet[1] != 0x20)) {
        return UhppotedError.InvalidReplyStartOfMessage;
    }

    if (packet[1] != 0x94) {
        return UhppotedError.InvalidReplyFunctionCode;
    }

    const controller = GetControllerResponse{
        .controller = unpack_uint32(packet, 4),
    };

    return controller;
}

fn unpack_uint32(packet: [64]u8, offset: u8) u32 {
    // nightlies:   std.mem.readIntLittle(u32, &packet[offset]);
    return std.mem.readIntLittle(u32, packet[offset..][0..4]);
}
