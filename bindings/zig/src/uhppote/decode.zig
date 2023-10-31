const std = @import("std");
const network = @import("zig-network");
const datelib = @import("datetime.zig");
const errors = @import("errors.zig");

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

    return std.mem.readIntLittle(u16, &slice);
}

fn unpack_uint32(packet: [64]u8, offset: u8) u32 {
    var slice = [4]u8{ packet[offset], packet[offset+1], packet[offset+2], packet[offset+3] };

    return std.mem.readIntLittle(u32, &slice);
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

fn unpack_date(packet: [64]u8, offset: u8) datelib.Date {
    const bcd = bcd2string(packet[offset..][0..4]);

    if (bcd) |string| {
        const year = std.fmt.parseUnsigned(u16, string[0..4], 10) catch 1900;
        const month = std.fmt.parseUnsigned(u8, string[4..6], 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, string[6..8], 10) catch 1;

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
    const bcd = bcd2string(packet[offset..][0..3]);

    if (bcd) |string| {
        // ... workaround for weird, obscure error in Zig v0.11.0 that corrupts string somehow
        const yy = [2]u8{ string[0], string[1] };
        const mm = [2]u8{ string[2], string[3] };
        const dd = [2]u8{ string[4], string[5] };

        const year = std.fmt.parseUnsigned(u16, &yy, 10) catch 0;
        const month = std.fmt.parseUnsigned(u8, &mm, 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, &dd, 10) catch 1;

        // const year = std.fmt.parseUnsigned(u16, string[0..2], 10) catch 0;
        // const month = std.fmt.parseUnsigned(u8, string[2..4], 10) catch 1;
        // const day = std.fmt.parseUnsigned(u8, string[4..6], 10) catch 1;

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
    const bcd = bcd2string(packet[offset..][0..4]);

    if (bcd) |string| {
        const year = std.fmt.parseUnsigned(u16, string[0..4], 10) catch 1900;
        const month = std.fmt.parseUnsigned(u8, string[4..6], 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, string[6..8], 10) catch 1;

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
    const bcd = bcd2string(packet[offset..][0..3]);

    if (bcd) |string| {
        // ... workaround for weird, obscure error in Zig v0.11.0 that corrupts string somehow
        const hh = [2]u8{ string[0], string[1] };
        const mm = [2]u8{ string[2], string[3] };
        const ss = [2]u8{ string[4], string[5] };

        const hour = std.fmt.parseUnsigned(u8, &hh, 10) catch 0;
        const minute = std.fmt.parseUnsigned(u8, &mm, 10) catch 0;
        const second = std.fmt.parseUnsigned(u8, &ss, 10) catch 0;

        // const hour = std.fmt.parseUnsigned(u8, string[0..2], 10) catch 0;
        // const minute = std.fmt.parseUnsigned(u8, string[2..4], 10) catch 0;
        // const second = std.fmt.parseUnsigned(u8, string[4..6], 10) catch 0;

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
    const bcd = bcd2string(packet[offset..][0..2]);

    if (bcd) |string| {
        const hour = std.fmt.parseUnsigned(u8, string[0..2], 10) catch 0;
        const minute = std.fmt.parseUnsigned(u8, string[2..4], 10) catch 0;

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
    const bcd = bcd2string(packet[offset..][0..7]);

    if (bcd) |string| {
        // ... workaround for weird, obscure error in Zig v0.11.0 that corrupts string somehow
        const yy = [4]u8{ string[0], string[1], string[2], string[3] };
        const MM = [2]u8{ string[4], string[5] };
        const dd = [2]u8{ string[6], string[7] };
        const hh = [2]u8{ string[8], string[9] };
        const mm = [2]u8{ string[10], string[11] };
        const ss = [2]u8{ string[12], string[13] };

        const year = std.fmt.parseUnsigned(u16, &yy, 10) catch 1900;
        const month = std.fmt.parseUnsigned(u8, &MM, 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, &dd, 10) catch 1;
        const hour = std.fmt.parseUnsigned(u8, &hh, 10) catch 0;
        const minute = std.fmt.parseUnsigned(u8, &mm, 10) catch 0;
        const second = std.fmt.parseUnsigned(u8, &ss, 10) catch 0;

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
            .year = 1900,
            .month = 1,
            .day = 1,
            .hour = 0,
            .minute = 0,
            .second = 0,
        };
    }
}

fn unpack_optional_datetime(packet: [64]u8, offset: u8) ?datelib.DateTime {
    const bcd = bcd2string(packet[offset..][0..7]);

    if (bcd) |string| {

        if (std.mem.eql(u8, string,&ZERO_DATETIME)) {
            return null;
        } else {
            // ... workaround for weird, obscure error in Zig v0.11.0 that corrupts string somehow
            const yy = [4]u8{ string[0], string[1], string[2], string[3] };
            const MM = [2]u8{ string[4], string[5] };
            const dd = [2]u8{ string[6], string[7] };
            const hh = [2]u8{ string[8], string[9] };
            const mm = [2]u8{ string[10], string[11] };
            const ss = [2]u8{ string[12], string[13] };

            const year = std.fmt.parseUnsigned(u16, &yy, 10) catch 1900;
            const month = std.fmt.parseUnsigned(u8, &MM, 10) catch 1;
            const day = std.fmt.parseUnsigned(u8, &dd, 10) catch 1;
            const hour = std.fmt.parseUnsigned(u8, &hh, 10) catch 0;
            const minute = std.fmt.parseUnsigned(u8, &mm, 10) catch 0;
            const second = std.fmt.parseUnsigned(u8, &ss, 10) catch 0;

            // const year = std.fmt.parseUnsigned(u16, string[0..4], 10) catch 1900;
            // const month = std.fmt.parseUnsigned(u8, string[4..6], 10) catch 1;
            // const day = std.fmt.parseUnsigned(u8, string[6..8], 10) catch 1;
            // const hour = std.fmt.parseUnsigned(u8, string[8..10], 10) catch 0;
            // const minute = std.fmt.parseUnsigned(u8, string[10..12], 10) catch 0;
            // const second = std.fmt.parseUnsigned(u8, string[12..14], 10) catch 0;

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

    return std.mem.readIntLittle(u24, &slice);
}

fn bcd2string(slice: []const u8) ![]u8 {
    var buffer: [64:0]u8 = undefined;

    return try std.fmt.bufPrintZ(&buffer, "{s}", .{std.fmt.fmtSliceHexLower(slice)});
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

    try std.testing.expect(response.controller == 405419896);
    try std.testing.expect(response.system_date.year == 2022);
    try std.testing.expect(response.system_date.month == 8);
    try std.testing.expect(response.system_date.day == 23);
    try std.testing.expect(response.system_time.hour == 9);
    try std.testing.expect(response.system_time.minute == 49);
    try std.testing.expect(response.system_time.second == 39);
    try std.testing.expect(response.door_1_open == false);
    try std.testing.expect(response.door_2_open == true);
    try std.testing.expect(response.door_3_open == false);
    try std.testing.expect(response.door_4_open == false);
    try std.testing.expect(response.door_1_button == false);
    try std.testing.expect(response.door_2_button == false);
    try std.testing.expect(response.door_3_button == false);
    try std.testing.expect(response.door_4_button  == true);
    try std.testing.expect(response.relays == 7);
    try std.testing.expect(response.inputs == 9);
    try std.testing.expect(response.system_error == 3);
    try std.testing.expect(response.special_info == 39);
    try std.testing.expect(response.event_index == 78);
    try std.testing.expect(response.event_type == 2);
    try std.testing.expect(response.event_access_granted == true);
    try std.testing.expect(response.event_door == 3);
    try std.testing.expect(response.event_direction == 1);
    try std.testing.expect(response.event_card == 8165537);

    if (response.event_timestamp) |timestamp| {
        try std.testing.expect(timestamp.year == 2022);    
        try std.testing.expect(timestamp.month == 8);
        try std.testing.expect(timestamp.day == 23);
        try std.testing.expect(timestamp.hour == 9);
        try std.testing.expect(timestamp.minute == 47);
        try std.testing.expect(timestamp.second == 6);
    } else {
        try std.testing.expect(false);
    }

    try std.testing.expect(response.event_reason == 44);
    try std.testing.expect(response.sequence_no == 0);
}

test "decode status response with no event" {
    const reply = [_]u8{
        0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    const response = try get_status_response(reply);

    try std.testing.expect(response.controller == 405419896);
    try std.testing.expect(response.system_date.year == 2022);
    try std.testing.expect(response.system_date.month == 8);
    try std.testing.expect(response.system_date.day == 23);
    try std.testing.expect(response.system_time.hour == 9);
    try std.testing.expect(response.system_time.minute == 49);
    try std.testing.expect(response.system_time.second == 39);
    try std.testing.expect(response.door_1_open == false);
    try std.testing.expect(response.door_2_open == true);
    try std.testing.expect(response.door_3_open == false);
    try std.testing.expect(response.door_4_open == false);
    try std.testing.expect(response.door_1_button == false);
    try std.testing.expect(response.door_2_button == false);
    try std.testing.expect(response.door_3_button == false);
    try std.testing.expect(response.door_4_button  == true);
    try std.testing.expect(response.relays == 7);
    try std.testing.expect(response.inputs == 9);
    try std.testing.expect(response.system_error == 3);
    try std.testing.expect(response.special_info == 39);
    try std.testing.expect(response.event_index == 0);
    try std.testing.expect(response.event_type == 0);
    try std.testing.expect(response.event_access_granted == false);
    try std.testing.expect(response.event_door == 0);
    try std.testing.expect(response.event_direction == 0);
    try std.testing.expect(response.event_card == 0);

    if (response.event_timestamp) |_| {
        try std.testing.expect(false);
        // try std.testing.expect(timestamp.year == 2022);    
        // try std.testing.expect(timestamp.month == 8);
        // try std.testing.expect(timestamp.day == 23);
        // try std.testing.expect(timestamp.hour == 9);
        // try std.testing.expect(timestamp.minute == 47);
        // try std.testing.expect(timestamp.second == 6);
    } else {
        try std.testing.expect(true);
    }

    try std.testing.expect(response.event_reason == 0);
    try std.testing.expect(response.sequence_no == 0);
}
