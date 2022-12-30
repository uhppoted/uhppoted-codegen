const std = @import("std");
const encode = @import("encode.zig");
const decode = @import("decode.zig");
const udp = @import("udp.zig");

pub fn set_debug(v: bool) void {
    udp.set_debug(v);
}

pub fn get_all_controllers(allocator: std.mem.Allocator) ![]decode.GetControllerResponse {
    const request = try encode.get_controller_request(0);
    const replies = try udp.broadcast(request, allocator);

    defer allocator.free(replies);

    var list = std.ArrayList(decode.GetControllerResponse).init(allocator);
    defer list.deinit();

    for (replies) |reply| {
        const response = try decode.get_controller_response(reply);

        try list.append(response);
    }

    return list.toOwnedSlice();
}

pub fn get_controller(device_id: u32, allocator: std.mem.Allocator) !decode.GetControllerResponse {
    const request = try encode.get_controller_request(device_id);
    const reply = try udp.send(request, allocator);

    return try decode.get_controller_response(reply);
}

pub fn listen(allocator: std.mem.Allocator) !void {
    try udp.listen(allocator);
}
