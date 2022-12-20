const std = @import("std");

pub const Command = struct { name: []const u8 };

pub const commands: [2]Command = [_]Command{
    Command{
        .name = "get-all-controllers",
    },

    Command{
        .name = "get-controller",
    },
};

pub fn exec(cmd: Command) !void {
    std.debug.print(">>>>>>>> EXEC {s}\n", .{cmd.name});
}
