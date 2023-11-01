const std = @import("std");
const network = @import("network");

const commands = @import("commands.zig");
const uhppote = @import("uhppote/uhppote.zig");

pub fn main() !void {
    // ... initialisation
    const w = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(w);
    const stdout = bw.writer();

    try stdout.print("uhppoted-codegen: Zig sample application\n", .{});
    try bw.flush();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // ... command line args
    var bind: [:0]const u8 = "0.0.0.0:0";
    var broadcast: [:0]const u8 = "255.255.255.255.60000";
    var listen: [:0]const u8 = "0.0.0.0:60001";
    var debug = false;

    var list = std.ArrayList([]const u8).init(allocator);
    defer list.deinit();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    if (args.next() != null) {
        while (args.next()) |arg| {
            if (std.mem.eql(u8, arg, "--bind")) {
                if (args.next()) |v| {
                    bind = v;
                }
            } else if (std.mem.eql(u8, arg, "--broadcast")) {
                if (args.next()) |v| {
                    broadcast = v;
                }
            } else if (std.mem.eql(u8, arg, "--listen")) {
                if (args.next()) |v| {
                    listen = v;
                }
            } else if (std.mem.eql(u8, arg, "--debug")) {
                debug = true;
            } else {
                try list.append(arg);
                while (args.next()) |a| {
                    try list.append(a);
                }
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
        try usage();
        return;
    }

    if (list.items.len == 1 and std.mem.eql(u8, list.items[0], "all")) {
        for (commands.commands) |v| {
            if (!std.mem.eql(u8, "listen", v.name)) {
                try commands.exec(v);            
            }
        }
        return;
    }

    for (list.items) |item| {
        for (commands.commands) |v| {
            if (std.mem.eql(u8, item, v.name)) {
                try commands.exec(v);
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

fn usage() !void {
    const w = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(w);
    const stdout = bw.writer();

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

    try bw.flush();
}
