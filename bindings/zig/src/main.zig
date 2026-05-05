const std = @import("std");

const commands = @import("commands.zig");
const uhppote = @import("uhppote/uhppote.zig");
const network = @import("uhppote/network.zig");

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa; 
    const io = init.io;

    var buffer: [4096]u8 = undefined;
    var w = std.Io.File.stdout().writer(io, &buffer);
    const stdout = &w.interface;

    try stdout.print("uhppoted-codegen: Zig sample application\n", .{});
    try stdout.flush();

    try commands.init(allocator);
    defer commands.deinit(allocator);

    // ... command line args
    var bind: [:0]const u8 = "0.0.0.0:0";
    var broadcast: [:0]const u8 = "255.255.255.255.60000";
    var listen: [:0]const u8 = "0.0.0.0:60001";
    var debug = false;

    var list = std.ArrayList([]const u8).empty;
    defer list.deinit(allocator);

    var it = try init.minimal.args.iterateAllocator(init.gpa);    
    defer it.deinit();

    _ = it.next(); 
    while (it.next()) |arg| {
        if (std.mem.eql(u8, arg, "--bind")) {
            if (it.next()) |v| {
                bind = v;
            }
        } else if (std.mem.eql(u8, arg, "--broadcast")) {
            if (it.next()) |v| {
                broadcast = v;
            }
        } else if (std.mem.eql(u8, arg, "--listen")) {
            if (it.next()) |v| {
                listen = v;
            }
        } else if (std.mem.eql(u8, arg, "--debug")) {
            debug = true;
        } else {
            try list.append(allocator, arg);
            while (it.next()) |a| {
                try list.append(allocator, a);
            }
        }
    }

    try uhppote.set_bind_address(bind);
    try uhppote.set_broadcast_address(broadcast);
    try uhppote.set_listen_address(listen);
    try uhppote.set_debug(debug);

    // ... execute commands
    try network.init();
    defer network.deinit();

    if (list.items.len == 0) {
        try usage(io);
        return;
    }

    if (list.items.len == 1 and std.mem.eql(u8, list.items[0], "all")) {
        for (commands.commands) |v| {
            if (!std.mem.eql(u8, "listen", v.name)) {
                try commands.exec(v, allocator);            
            }
        }
        return;
    }

    for (list.items) |item| {
        for (commands.commands) |v| {
            if (std.mem.eql(u8, item, v.name)) {
                try commands.exec(v, allocator);
                break;
            }
        } else {
            std.debug.print("\n", .{});
            std.debug.print("   *** Invalid command: '{s}'\n", .{item});
            std.debug.print("\n", .{});
            break;
        }
    }
}

fn usage(io: std.Io) !void {
    var buffer: [4096]u8 = undefined;
    var w = std.Io.File.stdout().writer(io, &buffer);
    const stdout = &w.interface;

    try stdout.print("\n", .{});
    try stdout.print("  Usage: zig build run [--debug] [--bind <address>] [--broadcast <address>] [--listen <address>] [commands]\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("    Options:\n", .{});
    try stdout.print("    --debug                Displays sent and received UDP packets\n", .{});
    try stdout.print("    --bind <address>       IPv4 address to which to bind. Defaults to 0.0.0.0\n", .{});
    try stdout.print("    --broadcast <address>  IPv4 address to which for UDP broadcast. Defaults to 255.255.255.255:60000\n", .{});
    try stdout.print("    --listen <address>     IPv4 address on which to listen for controller events. Defaults to 0.0.0.0.0:60001\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("    Commands:\n", .{});

    for (commands.commands) |v| {
        try stdout.print("      {s}\n", .{v.name});
    }

    try stdout.print("\n", .{});
    try stdout.flush();
}
