const std = @import("std");
const network = @import("zig-network");
const datelib = @import("datetime.zig");

const MAGIC: u32 = 0x55aaaa55;

{{range .model.requests}}
{{- template "request" . -}}
{{end}}

{{define "request"}}
pub fn {{snakeCase .name}}({{template "args" .fields}}) ![64]u8 {
    var packet = [_]u8{0} ** 64;

    packet[0] = 0x17;
    packet[1] = {{byte2hex .msgtype}};
    {{range .fields -}}
    {{if ne .type "magic"}}
    try pack_{{snakeCase .type}}({{snakeCase .name}}, &packet, {{.offset}});
    {{- else}}
    try pack_uint32(MAGIC, &packet, {{.offset}});
    {{- end}}{{end}}
 
    return packet;
}
{{end}}

fn pack_bool(v: bool, packet: *[64]u8, offset: u8) void {
    if (v) {
        packet[offset] = 1;
    } else {
        packet[offset] = 0;    
    }
}

fn pack_uint8(v: u8, packet: *[64]u8, offset: u8) !void {
    std.mem.writeIntLittle(u8, &packet[offset], v);
}

fn pack_uint16(v: u16, packet: *[64]u8, offset: u8) !void {
    std.mem.writeIntLittle(u16, &packet[offset], v);
}

fn pack_uint32(v: u32, packet: *[64]u8, offset: u8) !void {
    std.mem.writeIntLittle(u32, &packet[offset], v);
}

fn pack_ipv4(_: network.Address, _: *[64]u8, _: u8) !void {
}

fn pack_datetime(_: datelib.DateTime, _: *[64]u8, _: u8) !void {
}

fn pack_date(_: datelib.Date, _: *[64]u8, _: u8) !void {
}

fn pack_hhmm(_: datelib.Time, _: *[64]u8, _: u8) !void {
}

