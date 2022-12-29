const std = @import("std");
const network = @import("zig-network");
const datetime = @import("zig-date");

const UhppotedError = error{ InvalidReplyStartOfMessage, InvalidReplyFunctionCode };

pub const GetControllerResponse = struct {
    controller: u32,
    ip_address: network.Address.IPv4,
    subnet_mask: network.Address.IPv4,
    gateway: network.Address.IPv4,
    mac_address: [6]u8,
    version: [2]u8,
    date: datetime.Date,
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
        .ip_address = unpack_ipv4(packet, 8),
        .subnet_mask = unpack_ipv4(packet, 12),
        .gateway = unpack_ipv4(packet, 16),
        .mac_address = unpack_mac(packet, 20),
        .version = unpack_version(packet, 26),
        .date = unpack_date(packet, 28),
    };

    return controller;
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
        const s = "20190815";

        std.debug.print("{any} {any}\n", .{ string, s });

        var buffer = std.io.fixedBufferStream(s);
        var reader = buffer.reader();
        const d = datetime.Date.parseFmt("YMD", reader);

        if (d) |dt| {
            std.debug.print("{any}\n", .{dt});
        } else |err| {
            std.debug.print("{any}\n", .{err});
        }
    } else |err| {
        std.debug.print("{any}\n", .{err});
    }

    return datetime.Date.init(2022, 12, 29);
}

fn bcd2string(slice: []const u8) ![:0]u8 {
    var buffer: [64:0]u8 = undefined;

    return try std.fmt.bufPrintZ(&buffer, "{s}", .{std.fmt.fmtSliceHexLower(slice)});
}
