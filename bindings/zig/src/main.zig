const std = @import("std");
const commands = @import("commands.zig");

pub fn main() !void {
    // ... start message
    const w = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(w);
    const stdout = bw.writer();

    try stdout.print("uhppoted-codegen: Zig sample application\n", .{});
    try bw.flush();

    // ... command line args
    var bind = @as([*:0]const u8, "0.0.0.0:0");
    var broadcast = @as([*:0]const u8, "255.255.255.255.60000");
    var listen = @as([*:0]const u8, "0.0.0.0:60001");
    var debug = false;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

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
                break;
            }
        }
    }

    std.debug.print("BIND:      {s}\n", .{bind});
    std.debug.print("BROADCAST: {s}\n", .{broadcast});
    std.debug.print("LISTEN:    {s}\n", .{listen});
    std.debug.print("DEBUG:     {any}\n", .{debug});

    // ... execute command

    if (args.next()) |cmd| {
        std.debug.print("CMD: {s}\n", .{cmd});
    } else {
        try usage();
    }
}

fn usage() !void {
    const w = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(w);
    const stdout = bw.writer();

    try stdout.print("\n", .{});
    try stdout.print("  Usage: zig build run [--debug] [--bind <address>] [--broadcast <address>] [--listen <address>] [command]\n", .{});
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
