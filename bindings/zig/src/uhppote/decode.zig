const std = @import("std");
const network = @import("zig-network");
const datelib = @import("datetime.zig");
const errors = @import("errors.zig");

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

// pub fn get_event(packet: [64]u8) !Event {
//     // Ref. v6.62 firmware event
//     if ((packet[0] != 0x17) and (packet[0] != 0x19 or packet[1] != 0x20)) {
//         return errors.UhppotedError.InvalidReplyStartOfMessage;
//     }
// 
//     if (packet[1] != 0x20) {
//         return errors.UhppotedError.InvalidEventFunctionCode;
//     }
// 
//     const event = Event{};
// 
//     return event;
// }

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
    return std.mem.readIntLittle(u16, &packet[offset]);
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
        const year = std.fmt.parseUnsigned(u16, string[0..2], 10) catch 0;
        const month = std.fmt.parseUnsigned(u8, string[2..4], 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, string[4..6], 10) catch 1;

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

        return &datelib.Date{
            .year = year,
            .month = month,
            .day = day,
        };
    }
}

fn unpack_time(packet: [64]u8, offset: u8) datelib.Time {
    const bcd = bcd2string(packet[offset..][0..3]);

    if (bcd) |string| {
        const hour = std.fmt.parseUnsigned(u8, string[0..2], 10) catch 0;
        const minute = std.fmt.parseUnsigned(u8, string[2..4], 10) catch 0;
        const second = std.fmt.parseUnsigned(u8, string[4..6], 10) catch 0;

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
        const year = std.fmt.parseUnsigned(u16, string[0..4], 10) catch 1900;
        const month = std.fmt.parseUnsigned(u8, string[4..6], 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, string[6..8], 10) catch 1;
        const hour = std.fmt.parseUnsigned(u8, string[8..10], 10) catch 0;
        const minute = std.fmt.parseUnsigned(u8, string[10..12], 10) catch 0;
        const second = std.fmt.parseUnsigned(u8, string[12..14], 10) catch 0;

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
        const year = std.fmt.parseUnsigned(u16, string[0..4], 10) catch 1900;
        const month = std.fmt.parseUnsigned(u8, string[4..6], 10) catch 1;
        const day = std.fmt.parseUnsigned(u8, string[6..8], 10) catch 1;
        const hour = std.fmt.parseUnsigned(u8, string[8..10], 10) catch 0;
        const minute = std.fmt.parseUnsigned(u8, string[10..12], 10) catch 0;
        const second = std.fmt.parseUnsigned(u8, string[12..14], 10) catch 0;

        return datelib.DateTime{
            .year = year,
            .month = month,
            .day = day,
            .hour = hour,
            .minute = minute,
            .second = second,
        };
    }
}

fn bcd2string(slice: []const u8) ![]u8 {
    var buffer: [64:0]u8 = undefined;

    return try std.fmt.bufPrintZ(&buffer, "{s}", .{std.fmt.fmtSliceHexLower(slice)});
}
