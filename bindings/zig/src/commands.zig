const std = @import("std");
const uhppote = @import("uhppote/uhppote.zig");

pub const Command = struct {
    name: []const u8,
    function: *const fn (std.mem.Allocator) void,
};

pub const commands: [2]Command = [_]Command{
    Command{
        .name = "get-all-controllers",
        .function = get_all_controllers,
    },

    Command{
        .name = "get-controller",
        .function = get_controller,
    },
};

pub fn exec(cmd: Command) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    cmd.function(allocator);
}

fn get_all_controllers(allocator: std.mem.Allocator) void {
    var list = uhppote.get_all_controllers(allocator);
    if (list) |l| {
        for (l) |item| {
            std.debug.print("{any}\n", .{item});
        }

        allocator.free(l);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_controller(allocator: std.mem.Allocator) void {
    var response = uhppote.get_controller(405419896, allocator);

    std.debug.print("{any}\n", .{response});
}
