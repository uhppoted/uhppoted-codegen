const std = @import("std");
const datelib = @import("datetime.zig");

const encode = @import("encode.zig");
const decode = @import("decode.zig");
const ut0311 = @import("ut0311.zig");
const network = @import("network.zig");

pub const Controller = ut0311.Controller;

pub fn set_bind_address(addr: [:0]const u8) !void {
    try ut0311.set_bind_address(addr);
}

pub fn set_broadcast_address(addr: [:0]const u8) !void {
    try ut0311.set_broadcast_address(addr);
}

pub fn set_listen_address(addr: [:0]const u8) !void {
    try ut0311.set_listen_address(addr);
}

pub fn set_debug(v: bool) !void {
    try ut0311.set_debug(v);
}

// FIXME: using threads in lieu of async because async is currently broken in the nightlies
pub fn listen(handler: *const fn (decode.Event) void, allocator: std.mem.Allocator, io: std.Io) !void {
    var queue = std.ArrayListUnmanaged([64]u8){
        .items = &.{},
        .capacity = 0,
    };
  
    defer queue.deinit(allocator);

    const ctx = context{
        .q = &queue,
        .allocator = allocator,
    };

    var thread = try std.Thread.spawn(.{}, on_event, .{@as(context, ctx)});

    while (true) {
    while (queue.items.len > 0){
            const packet = queue.orderedRemove(0);
            const event = decode.get_event(packet);

            if (event) |e| {
                handler(e);
            } else |err| {
                std.debug.print("\n   *** ERROR  {any}\n", .{err});
            }
        }

        // Ewwww :-( .. must be some way to block/wait on a queue?
        io.sleep(.fromMilliseconds(1000), .monotonic) catch {};
    }

    thread.join();
}

// pub fn listen(handler: *const fn (decode.Event) void, allocator: std.mem.Allocator) !void {
//     var queue = std.ArrayListUnmanaged([64]u8).empty;
//     defer queue.deinit(allocator);
// 
//     var ctx = context{
//         .q = &queue,
//         .allocator = allocator,
//     };
// 
//     var thread = try std.Thread.spawn(.{}, on_event, .{&ctx});
//     defer thread.join();
// 
//     while (true) {
//         ctx.mutex.lock();
//         while (ctx.q.items.len == 0) {
//             ctx.cond.wait(&ctx.mutex);
//         }
//         const packet = ctx.q.orderedRemove(0);
//         ctx.mutex.unlock();
// 
//         const event = decode.get_event(packet);
//         if (event) |e| {
//             handler(e);
//         } else |err| {
//             std.debug.print("\n   *** ERROR  {any}\n", .{err});
//         }
//     }
// }

const context = struct {
    q: *std.ArrayListUnmanaged([64]u8),
    allocator: std.mem.Allocator,
//  semaphore: std.Thread.Semaphore = .{},
//  cond: std.Thread.Condition = .{},
};

fn on_event(ctx: context) void {
    if (ut0311.listen(ctx.q, ctx.allocator)) {} else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

pub fn get_all_controllers(allocator: std.mem.Allocator) ![]decode.GetControllerResponse {
    const request = try encode.get_controller_request(0);
    const replies = try ut0311.broadcast(request, allocator);

    defer allocator.free(replies);

    var list = std.ArrayListUnmanaged(decode.GetControllerResponse){};
    defer list.deinit(allocator);

    for (replies) |reply| {
        const response = try decode.get_controller_response(reply);

        try list.append(allocator, response);
    }

    return list.toOwnedSlice(allocator);
}

{{range .model.functions}}
{{- template "function" . -}}
{{end -}}

{{define "function"}}
pub fn {{snakeCase .name}}({{template "args" .args}}, allocator: std.mem.Allocator) {{if .response -}}!decode.{{template "result" .}}{{else}}!bool{{end}} { 
    const request = try encode.{{snakeCase .request.name}}(controller.controller, {{template "params" slice .args 1}});
    {{if .response -}}
    const reply = try ut0311.send(controller, request, allocator);

    return try decode.{{snakeCase .response.name}}(reply);
    {{else}}
    _ = try ut0311.send(controller, request, allocator);

    return true;
    {{end}}
}
{{end}}
