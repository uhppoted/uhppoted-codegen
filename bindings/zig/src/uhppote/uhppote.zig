const std = @import("std");
const datelib = @import("datetime.zig");

const encode = @import("encode.zig");
const decode = @import("decode.zig");
const udp = @import("udp.zig");
const network = @import("network.zig");

pub fn set_bind_address(addr: [:0]const u8) !void {
    try udp.set_bind_address(addr);
}

pub fn set_broadcast_address(addr: [:0]const u8) !void {
    try udp.set_broadcast_address(addr);
}

pub fn set_listen_address(addr: [:0]const u8) !void {
    try udp.set_listen_address(addr);
}

pub fn set_debug(v: bool) !void {
    try udp.set_debug(v);
}

// FIXME: using threads in lieu of async because async is currently broken in the nightlies
pub fn listen(handler: *const fn (decode.Event) void, allocator: std.mem.Allocator) !void {
    var queue = std.atomic.Queue([64]u8).init();

    const ctx = context{
        .q = &queue,
        .allocator = allocator,
    };

    var thread = try std.Thread.spawn(.{}, on_event, .{@as(context, ctx)});

    while (true) {
        while (queue.get()) |node| {
            const packet = node.data;
            const event = decode.get_event(packet);

            if (event) |e| {
                handler(e);
            } else |err| {
                std.debug.print("\n   *** ERROR  {any}\n", .{err});
            }
        }

        // Ewwww :-( .. must be some way to block/wait on a queue?
        std.time.sleep(1000 * std.time.ns_per_ms);
    }

    thread.join();
}

const context = struct {
    q: *std.atomic.Queue([64]u8),
    allocator: std.mem.Allocator,
};

fn on_event(ctx: context) void {
    if (udp.listen(ctx.q, ctx.allocator)) {} else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
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

{{range .model.functions}}
{{- template "function" . -}}
{{end -}}

{{define "function"}}
pub fn {{snakeCase .name}}({{template "args" .args}}, allocator: std.mem.Allocator) {{if .response -}}!decode.{{template "result" .}}{{else}}!bool{{end}} { 
    const request = try encode.{{snakeCase .request.name}}({{template "params" .args}});
    {{if .response -}}
    const reply = try udp.send(request, allocator);

    return try decode.{{snakeCase .response.name}}(reply);
    {{else}}
    _ = try udp.send(request, allocator);

    return true;
    {{end}}
}
{{end}}
