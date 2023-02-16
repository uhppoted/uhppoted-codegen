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

fn pack_bool(v: bool, packet: *[64]u8, offset: u8) !void {
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

fn pack_ipv4(v: network.Address.IPv4, packet: *[64]u8, offset: u8) !void {
    packet[offset] = v.value[0];
    packet[offset+1] = v.value[1];
    packet[offset+2] = v.value[2];
    packet[offset+3] = v.value[3];
}

fn pack_datetime(v: datelib.DateTime, packet: *[64]u8, offset: u8) !void {
    var buffer: [64:0]u8 = undefined;
    const s = try std.fmt.bufPrintZ(&buffer, 
                                    "{d:0>4}{d:0>2}{d:0>2}{d:0>2}{d:0>2}{d:0>2}", 
                                    .{v.year,v.month,v.day,v.hour,v.minute,v.second});

    var bcd = [_]u8{0} ** 7;
    _ = try string2bcd(s,&bcd);

    packet[offset] = bcd[0];
    packet[offset+1] = bcd[1];
    packet[offset+2] = bcd[2];
    packet[offset+3] = bcd[3];
    packet[offset+4] = bcd[4];
    packet[offset+5] = bcd[5];
    packet[offset+6] = bcd[6];
}

fn pack_date(v: datelib.Date, packet: *[64]u8, offset: u8) !void {
    var buffer: [64:0]u8 = undefined;
    const s = try std.fmt.bufPrintZ(&buffer, 
                                    "{d:0>4}{d:0>2}{d:0>2}", 
                                    .{v.year,v.month,v.day});

    var bcd = [_]u8{0} ** 4;
    _ = try string2bcd(s,&bcd);

    packet[offset] = bcd[0];
    packet[offset+1] = bcd[1];
    packet[offset+2] = bcd[2];
    packet[offset+3] = bcd[3];
}

fn pack_hhmm(v: datelib.Time, packet: *[64]u8, offset: u8) !void {
    var buffer: [64:0]u8 = undefined;
    const s = try std.fmt.bufPrintZ(&buffer, 
                                    "{d:0>2}{d:0>2}", 
                                    .{v.hour,v.minute});

    var bcd = [_]u8{0} ** 2;
    _ = try string2bcd(s,&bcd);

    packet[offset] = bcd[0];
    packet[offset+1] = bcd[1];
}

fn pack_pin(v: u24, packet: *[64]u8, offset: u8) !void {
    std.mem.writeIntLittle(u24, &packet[offset], v);
}

fn string2bcd(v: []const u8, bcd: []u8) !void {
    var offset:u8 = 0;
    var index:u8 = 0;

    while (offset < v.len) {
        const byte = try std.fmt.parseUnsigned(u8, v[offset..][0..2], 16);
        bcd[index] = byte;
        offset += 2;
        index += 1;
    }
}
