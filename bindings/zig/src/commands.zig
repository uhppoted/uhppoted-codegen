pub const Command = struct { name: []const u8 };

pub const commands: [2]Command = [_]Command{
    Command{
        .name = "get-all-controllers",
    },

    Command{
        .name = "get-controller",
    },
};

