const std = @import("std");
const uhppote = @import("uhppote/uhppote.zig");

pub const Command = struct {
    name: []const u8,
    function: fn () void,
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
    cmd.function();
}

fn get_all_controllers() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

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

fn get_controller() void {
    std.debug.print("get-controller\n", .{});
}
