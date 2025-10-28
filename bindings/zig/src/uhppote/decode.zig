const std = @import("std");
const datelib = @import("datetime.zig");
const errors = @import("errors.zig");
const network = @import("network.zig");

const ZERO_DATETIME = [_]u8{ 48, 48, 48, 48, 48,48, 48, 48, 48, 48, 48, 48, 48, 48};

{{range .model.responses}}
{{- template "response" . -}}
{{end}}

{{- with .model.event}}
pub const {{CamelCase .name}} = struct { {{range .fields}}
     {{snakeCase .name}}: {{template "type" .type}},{{end}}
};
{{end}}

{{define "response"}}
pub const {{CamelCase .name}} = struct { {{range .fields}}
    {{snakeCase .name}}: {{template "type" .type}},{{end}}
};
{{end}}

{{- range .model.responses}}
{{- template "decode" . -}}
{{end}}

{{- with .model.event -}}
{{- template "decode" . -}}
{{end -}}

{{define "decode"}}
pub fn {{snakeCase .name}}(packet: [64]u8) !{{CamelCase .name}} {
    // Ref. v6.62 firmware event
    if ((packet[0] != 0x17) and (packet[0] != 0x19 or packet[1] != 0x20)) {
        return errors.UhppotedError.InvalidReplyStartOfMessage;
    }

    if (packet[1] != {{byte2hex .msgtype}}) {
        return errors.UhppotedError.InvalidEventFunctionCode;
    }
 
    const response = {{CamelCase .name}}{ {{range .fields}}
        .{{snakeCase .name}} = unpack_{{snakeCase .type}}(packet, {{.offset}}),{{end}}
    };

    return response;
}
{{end -}}

{{- with .model.event -}}
pub fn get_{{snakeCase .name}}(packet: [64]u8) !{{CamelCase .name}} {
    // Ref. v6.62 firmware event
    if ((packet[0] != 0x17) and (packet[0] != 0x19 or packet[1] != 0x20)) {
        return errors.UhppotedError.InvalidReplyStartOfMessage;
    }

    if (packet[1] != {{byte2hex .msgtype}}) {
        return errors.UhppotedError.InvalidEventFunctionCode;
    }
 
    const response = {{CamelCase .name}}{ {{range .fields}}
        .{{snakeCase .name}} = unpack_{{snakeCase .type}}(packet, {{.offset}}),{{end}}
    };

    return response;
}
{{end -}}

fn unpack_bool(packet: [64]u8, offset: u8) bool {
    if (packet[offset] == 1) {
        return true;
    } else {
        return false;
    }
}

fn unpack_uint8(packet: [64]u8, offset: u8) u8 {
    return packet[offset];
}

fn unpack_uint16(packet: [64]u8, offset: u8) u16 {
    var slice = [2]u8{ packet[offset], packet[offset+1] };

    return std.mem.readInt(u16, &slice,.little);
}

fn unpack_uint32(packet: [64]u8, offset: u8) u32 {
    var slice = [4]u8{ packet[offset], packet[offset+1], packet[offset+2], packet[offset+3] };

    return std.mem.readInt(u32, &slice,.little);
}

fn unpack_ipv4(packet: [64]u8, offset: u8) network.Address.IPv4 {
    return network.Address.IPv4.init(packet[offset], packet[offset + 1], packet[offset + 2], packet[offset + 3]);
}

fn unpack_mac(packet: [64]u8, offset: u8) [:0]const u8 {
    const allocator = std.heap.page_allocator;
    
    return std.fmt.allocPrintZ(allocator, "{x:0>2}:{x:0>2}:{x:0>2}:{x:0>2}:{x:0>2}:{x:0>2}",
                                             .{ 
                                                packet[offset], 
                                                packet[offset+1],
                                                packet[offset+2],
                                                packet[offset+3],
                                                packet[offset+4],
                                                packet[offset+5],
                                              }) catch "";
}

fn unpack_version(packet: [64]u8, offset: u8) [:0]const u8 {
    const allocator = std.heap.page_allocator;

    return std.fmt.allocPrintZ(allocator, "v{x:}.{x:0>2}", .{ packet[offset], packet[offset+1] }) catch "";
}

fn unpack_date(packet: [64]u8, offset: u8) datelib.Date {
    var bcd: [8]u8 = undefined;

    if (bcd2string(packet[offset..][0..4],&bcd)) |_| {
        const year = std.fmt.parseUnsigned(u16, bcd[0..4], 10) catch 2000;
        const month = std.fmt.parseUnsigned(u8, bcd[4..6], 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, bcd[6..8], 10) catch 1;

        return datelib.Date{
            .year = year,
            .month = month,
            .day = day,
        };
    } else |_| {
        return datelib.Date{
            .year = 1900,
            .month = 1,
            .day = 1,
        };
    }
}

fn unpack_shortdate(packet: [64]u8, offset: u8) datelib.Date {
    var bcd: [6]u8 = undefined;

    if (bcd2string(packet[offset..][0..3],&bcd)) |_| {
        const year = std.fmt.parseUnsigned(u16, bcd[0..2], 10) catch 0;
        const month = std.fmt.parseUnsigned(u8, bcd[2..4], 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, bcd[4..6], 10) catch 1;

        return datelib.Date{
            .year = 2000 + year,
            .month = month,
            .day = day,
        };
    } else |_| {
        return datelib.Date{
            .year = 1900,
            .month = 1,
            .day = 1,
        };
    }
}

fn unpack_optional_date(packet: [64]u8, offset: u8) ?datelib.Date {
    var bcd: [8]u8 = undefined;

    if (bcd2string(packet[offset..][0..4],&bcd)) |_| {
        const year = std.fmt.parseUnsigned(u16, bcd[0..4], 10) catch 2000;
        const month = std.fmt.parseUnsigned(u8, bcd[4..6], 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, bcd[6..8], 10) catch 1;

        return datelib.Date{
            .year = year,
            .month = month,
            .day = day,
        };
    } else |_| {
        return null;
    }
}

fn unpack_time(packet: [64]u8, offset: u8) datelib.Time {
    var bcd: [6]u8 = undefined;

    if (bcd2string(packet[offset..][0..3],&bcd)) |_| {
        const hour = std.fmt.parseUnsigned(u8, bcd[0..2], 10) catch 0;
        const minute = std.fmt.parseUnsigned(u8, bcd[2..4], 10) catch 0;
        const second = std.fmt.parseUnsigned(u8, bcd[4..6], 10) catch 0;

        return datelib.Time{
            .hour = hour,
            .minute = minute,
            .second = second,
        };
    } else |_| {
        return datelib.Time{
            .hour = 0,
            .minute = 0,
            .second = 0,
        };
    }
}

fn unpack_hhmm(packet: [64]u8, offset: u8) datelib.Time {
    var bcd: [4]u8 = undefined;

    if (bcd2string(packet[offset..][0..2],&bcd)) |_| {
        const hour = std.fmt.parseUnsigned(u8, bcd[0..2], 10) catch 0;
        const minute = std.fmt.parseUnsigned(u8, bcd[2..4], 10) catch 0;

        return datelib.Time{
            .hour = hour,
            .minute = minute,
            .second = 0,
        };
    } else |_| {
        return datelib.Time{
            .hour = 0,
            .minute = 0,
            .second = 0,
        };
    }
}

fn unpack_datetime(packet: [64]u8, offset: u8) datelib.DateTime {
    var bcd: [14]u8 = undefined;

    if (bcd2string(packet[offset..][0..7],&bcd)) |_| {
        const year = std.fmt.parseUnsigned(u16, bcd[0..4], 10) catch 2000;
        const month = std.fmt.parseUnsigned(u8, bcd[4..6], 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, bcd[6..8], 10) catch 1;
        const hour = std.fmt.parseUnsigned(u8, bcd[8..10], 10) catch 0;
        const minute = std.fmt.parseUnsigned(u8, bcd[10..12], 10) catch 0;
        const second = std.fmt.parseUnsigned(u8, bcd[12..14], 10) catch 0;

        return datelib.DateTime{
            .year = year,
            .month = month,
            .day = day,
            .hour = hour,
            .minute = minute,
            .second = second,
        };
    } else |_| {
        return datelib.DateTime{
            .year = 2000,
            .month = 1,
            .day = 1,
            .hour = 0,
            .minute = 0,
            .second = 0,
        };
    }
}

fn unpack_optional_datetime(packet: [64]u8, offset: u8) ?datelib.DateTime {
    var bcd: [14]u8 = undefined;

    if (bcd2string(packet[offset..][0..7],&bcd)) |_| {
        if (std.mem.eql(u8, &bcd,&ZERO_DATETIME)) {
            return null;
        } else {
            const year = std.fmt.parseUnsigned(u16, bcd[0..4], 10) catch 2000;
            const month = std.fmt.parseUnsigned(u8, bcd[4..6], 10) catch 1;
            const day = std.fmt.parseUnsigned(u8, bcd[6..8], 10) catch 1;
            const hour = std.fmt.parseUnsigned(u8, bcd[8..10], 10) catch 0;
            const minute = std.fmt.parseUnsigned(u8, bcd[10..12], 10) catch 0;
            const second = std.fmt.parseUnsigned(u8, bcd[12..14], 10) catch 0;

            return datelib.DateTime{
                .year = year,
                .month = month,
                .day = day,
                .hour = hour,
                .minute = minute,
                .second = second,
            };
        }
    } else |_| {
        return null;
    }
}

fn unpack_pin(packet: [64]u8, offset: u8) u24 {
    var slice = [3]u8{ packet[offset], packet[offset+1], packet[offset+2] };

    return std.mem.readInt(u24, &slice,.little);
}

fn bcd2string(slice: []const u8, buffer: []u8) !void {
    _ = try std.fmt.bufPrint(buffer, "{s}", .{std.fmt.fmtSliceHexLower(slice)});
}

// Unit tests

test "decode status response" {
    const reply = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
        0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const response = try get_status_response(reply);

    try std.testing.expectEqual(response.controller, 405419896);
    try std.testing.expectEqual(response.system_date, datelib.Date{ .year = 2022, .month = 8, .day = 23 });
    try std.testing.expectEqual(response.system_time, datelib.Time{ .hour = 9, .minute = 49, .second = 39 });
    try std.testing.expectEqual(response.door_1_open, false);
    try std.testing.expectEqual(response.door_2_open, true);
    try std.testing.expectEqual(response.door_3_open, false);
    try std.testing.expectEqual(response.door_4_open, false);
    try std.testing.expectEqual(response.door_1_button, false);
    try std.testing.expectEqual(response.door_2_button, false);
    try std.testing.expectEqual(response.door_3_button, false);
    try std.testing.expectEqual(response.door_4_button, true);
    try std.testing.expectEqual(response.relays, 7);
    try std.testing.expectEqual(response.inputs, 9);
    try std.testing.expectEqual(response.system_error, 3);
    try std.testing.expectEqual(response.special_info, 39);
    try std.testing.expectEqual(response.event_index, 78);
    try std.testing.expectEqual(response.event_type, 2);
    try std.testing.expectEqual(response.event_access_granted, true);
    try std.testing.expectEqual(response.event_door, 3);
    try std.testing.expectEqual(response.event_direction, 1);
    try std.testing.expectEqual(response.event_card, 8165537);
    try std.testing.expectEqual(response.event_timestamp, 
                                datelib.DateTime{ .year = 2022, .month = 8, .day = 23, .hour = 9, .minute = 47, .second = 6 });
    try std.testing.expectEqual(response.event_reason, 44);
    try std.testing.expectEqual(response.sequence_no, 0);
}

test "decode status response with no event" {
    const reply = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const response = try get_status_response(reply);

    try std.testing.expectEqual(response.controller, 405419896);
    try std.testing.expectEqual(response.system_date, datelib.Date{ .year = 2022, .month = 8, .day = 23 });
    try std.testing.expectEqual(response.system_time, datelib.Time{ .hour = 9, .minute = 49, .second = 39 });
    try std.testing.expectEqual(response.door_1_open, false);
    try std.testing.expectEqual(response.door_2_open, true);
    try std.testing.expectEqual(response.door_3_open, false);
    try std.testing.expectEqual(response.door_4_open, false);
    try std.testing.expectEqual(response.door_1_button, false);
    try std.testing.expectEqual(response.door_2_button, false);
    try std.testing.expectEqual(response.door_3_button, false);
    try std.testing.expectEqual(response.door_4_button, true);
    try std.testing.expectEqual(response.relays, 7);
    try std.testing.expectEqual(response.inputs, 9);
    try std.testing.expectEqual(response.system_error, 3);
    try std.testing.expectEqual(response.special_info, 39);
    try std.testing.expectEqual(response.event_index, 0);
    try std.testing.expectEqual(response.event_type, 0);
    try std.testing.expectEqual(response.event_access_granted, false);
    try std.testing.expectEqual(response.event_door, 0);
    try std.testing.expectEqual(response.event_direction, 0);
    try std.testing.expectEqual(response.event_card, 0);
    try std.testing.expectEqual(response.event_timestamp, null);
    try std.testing.expectEqual(response.event_reason, 0);
    try std.testing.expectEqual(response.sequence_no, 0);
}

test "unpack_date" {
    const packet = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
        0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const expected = datelib.Date{.year = 2022, .month=8, .day=23};
    const date = unpack_date(packet,20);

    try std.testing.expectEqual(expected, date);
}

test "unpack_shortdate" {
    const packet = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
        0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const expected = datelib.Date{.year = 2022, .month=8, .day=23};
    const date = unpack_shortdate(packet,21);

    try std.testing.expectEqual(expected, date);
}

test "unpack_optional_date" {
    const packet = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
        0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const expected = datelib.Date{.year = 2022, .month=8, .day=23};
    const date = unpack_optional_date(packet,20);

    try std.testing.expectEqual(expected, date.?);
}

test "unpack_time" {
    const packet = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
        0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const expected = datelib.Time{.hour = 9, .minute=47, .second=6};
    const time = unpack_time(packet,24);

    try std.testing.expectEqual(expected, time);
}

test "unpack_hhmm" {
    const packet = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
        0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const expected = datelib.Time{.hour = 9, .minute=47, .second=0};
    const HHmm = unpack_hhmm(packet,24);

    try std.testing.expectEqual(expected, HHmm);
}

test "unpack_datetime" {
    const packet = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
        0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const expected = datelib.DateTime{.year = 2022, .month=8, .day=23, .hour = 9, .minute=47, .second=6};
    const datetime = unpack_datetime(packet,20);

    try std.testing.expectEqual(expected, datetime);
}

test "unpack_optional_datetime" {
    const packet = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
        0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const expected = datelib.DateTime{.year = 2022, .month=8, .day=23, .hour = 9, .minute=47, .second=6};
    const datetime = unpack_optional_datetime(packet,20);

    try std.testing.expectEqual(expected, datetime.?);
}

fn unpack_mode(packet: [64]u8, offset: u8) u8 {
    return packet[offset];
}

fn unpack_anti_passback(packet: [64]u8, offset: u8) u8 {
    return packet[offset];
}

fn unpack_event_type(packet: [64]u8, offset: u8) u8 {
    return packet[offset];
}

fn unpack_direction(packet: [64]u8, offset: u8) u8 {
    return packet[offset];
}

fn unpack_reason(packet: [64]u8, offset: u8) u8 {
    return packet[offset];
}

test "bcd2string" {
    const packet = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
        0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const offset = 20;
    var bcd: [14]u8 = undefined;
    const expected: []const u8 = "20220823094706";

     _ = try bcd2string(packet[offset..][0..7],&bcd);

    try std.testing.expect(std.mem.eql(u8, expected,&bcd));
}

