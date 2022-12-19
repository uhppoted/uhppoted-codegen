const std = @import("std");

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
            }
        }
    }

    std.debug.print("BIND:      {s}\n", .{bind});
    std.debug.print("BROADCAST: {s}\n", .{broadcast});
    std.debug.print("LISTEN:    {s}\n", .{listen});
    std.debug.print("DEBUG:     {any}\n", .{debug});
}
