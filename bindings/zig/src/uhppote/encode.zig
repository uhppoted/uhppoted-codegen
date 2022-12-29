const std = @import("std");

pub fn get_controller_request(device_id: u32) ![64]u8 {
    std.debug.print("get-controller-request {any}\n", .{device_id});

    var packet = [_]u8{0} ** 64;

    packet[0] = 0x17;
    packet[1] = 0x94;

    pack_uint32(device_id, &packet, 4);

    return packet;
}

fn pack_uint32(v: u32, packet: *[64]u8, offset: u8) void {
    // nightlies:   std.mem.writeIntLittle(u32, &packet[offset], v);
    std.mem.writeIntLittle(u32, packet[offset..][0..4], v);
}
