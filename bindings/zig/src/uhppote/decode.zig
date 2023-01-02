const std = @import("std");
const network = @import("zig-network");
const datetime = @import("zig-date");
const errors = @import("errors.zig");

pub const GetControllerResponse = struct {
    controller: u32,
    ip_address: network.Address.IPv4,
    subnet_mask: network.Address.IPv4,
    gateway: network.Address.IPv4,
    mac_address: [6]u8,
    version: [2]u8,
    date: datetime.Date,
};

pub const Event = struct {};

pub fn get_controller_response(packet: [64]u8) !GetControllerResponse {
    // Ref. v6.62 firmware event
    if ((packet[0] != 0x17) and (packet[0] != 0x19 or packet[1] != 0x20)) {
        return errors.UhppotedError.InvalidReplyStartOfMessage;
    }

    if (packet[1] != 0x94) {
        return errors.UhppotedError.InvalidReplyFunctionCode;
    }

    const controller = GetControllerResponse{
        .controller = unpack_uint32(packet, 4),
        .ip_address = unpack_ipv4(packet, 8),
        .subnet_mask = unpack_ipv4(packet, 12),
        .gateway = unpack_ipv4(packet, 16),
        .mac_address = unpack_mac(packet, 20),
        .version = unpack_version(packet, 26),
        .date = unpack_date(packet, 28),
    };

    return controller;
}

pub fn get_event(packet: [64]u8) !Event {
    // Ref. v6.62 firmware event
    if ((packet[0] != 0x17) and (packet[0] != 0x19 or packet[1] != 0x20)) {
        return errors.UhppotedError.InvalidReplyStartOfMessage;
    }

    if (packet[1] != 0x20) {
        return errors.UhppotedError.InvalidEventFunctionCode;
    }

    const event = Event{};

    return event;
}

fn unpack_uint32(packet: [64]u8, offset: u8) u32 {
    return std.mem.readIntLittle(u32, &packet[offset]);
}

fn unpack_ipv4(packet: [64]u8, offset: u8) network.Address.IPv4 {
    return network.Address.IPv4.init(packet[offset], packet[offset + 1], packet[offset + 2], packet[offset + 3]);
}

fn unpack_mac(packet: [64]u8, offset: u8) [6]u8 {
    return packet[offset..][0..6].*;
}

fn unpack_version(packet: [64]u8, offset: u8) [2]u8 {
    return packet[offset..][0..2].*;
}

fn unpack_date(packet: [64]u8, offset: u8) datetime.Date {
    const bcd = bcd2string(packet[offset..][0..4]);

    if (bcd) |string| {
        var buffer = std.io.fixedBufferStream(string);
        var reader = buffer.reader();

        return datetime.Date.parseFmt("YMD", reader) catch return datetime.Date.init(1900, 1, 1);
    } else |_| {
        return datetime.Date.init(1900, 1, 1);
    }
}

fn bcd2string(slice: []const u8) ![]u8 {
    var buffer: [64:0]u8 = undefined;

    return try std.fmt.bufPrintZ(&buffer, "{s}", .{std.fmt.fmtSliceHexLower(slice)});
}
