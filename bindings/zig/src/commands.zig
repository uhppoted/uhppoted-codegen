const std = @import("std");
const network = @import("zig-network");

const uhppote = @import("uhppote/uhppote.zig");
const decode = @import("uhppote/decode.zig");

const CONTROLLER: u32 = 405419896;

pub const Command = struct {
    name: []const u8,
    function: *const fn (std.mem.Allocator) void,
};

pub const commands = [_]Command{
    Command{
        .name = "get-all-controllers",
        .function = get_all_controllers,
    },

    Command{
        .name = "get-controller",
        .function = get_controller,
    },

    Command {
        .name = "set-ip",
        .function = set_ip,
    },

//   Command {
//       .name = "get-time",
//       .function = get_time,
//   },
//   Command {
//       .name = "set-time",
//       .function = set_time,
//   },
//   Command {
//       .name = "get-status",
//       .function = get_status,
//   },
//   Command {
//       .name = "get-listener",
//       .function = get_listener,
//   },
//   Command {
//       .name = "set-listener",
//       .function = set_listener,
//   },
//   Command {
//       .name = "get-door-control",
//       .function = get_door_control,
//   },
//   Command {
//       .name = "set-door-control",
//       .function = set_door_control,
//   },
//   Command {
//       .name = "open-door",
//       .function = open_door,
//   },
//   Command {
//       .name = "get-cards",
//       .function = get_cards,
//   },
//   Command {
//       .name = "get-card",
//       .function = get_card,
//   },
//   Command {
//       .name = "get-card-by-index",
//       .function = get_card_by_index,
//   },
//   Command {
//       .name = "put-card",
//       .function = put_card,
//   },
//   Command {
//       .name = "delete-card",
//       .function = delete_card,
//   },
//   Command {
//       .name = "delete-all-cards",
//       .function = delete_all_cards,
//   },
//   Command {
//       .name = "get-event",
//       .function = get_event,
//   },
//   Command {
//       .name = "get-event-index",
//       .function = get_event_index,
//   },
//   Command {
//       .name = "set-event-index",
//       .function = set_event_index,
//   },
//   Command {
//       .name = "record-special-events",
//       .function = record_special_events,
//   },
//   Command {
//       .name = "get-time-profile",
//       .function = get_time_profile,
//   },
//   Command {
//       .name = "set-time-profile",
//       .function = set_time_profile,
//   },
//   Command {
//       .name = "delete-all-time-profiles",
//       .function = delete_all_time_profiles,
//   },
//   Command {
//       .name = "add-task",
//       .function = add_task,
//   },
//   Command {
//       .name = "refresh-tasklist",
//       .function = refresh_tasklist,
//   },
//   Command {
//       .name = "clear-tasklist",
//       .function = clear_tasklist,
//   },

    Command{
        .name = "listen",
        .function = listen,
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
        for (l) |controller| {
            pprint(controller);
        }

        allocator.free(l);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_controller(allocator: std.mem.Allocator) void {
    if (uhppote.get_controller(CONTROLLER, allocator)) |controller| {
        pprint(controller);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_ip(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;
    const address = network.Address.IPv4.init(192,168,1,100);
    const netmask = network.Address.IPv4.init(255,255,255,0);
    const gateway = network.Address.IPv4.init(192,168,1,1);

    if (uhppote.set_ip(controller, address, netmask, gateway, allocator)) |ok| {
        pprint(ok);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn listen(allocator: std.mem.Allocator) void {
    if (uhppote.listen(on_event, allocator)) {} else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn on_event(event: decode.Event) void {
    pprint(event);
}

fn pprint(v: anytype) void {
    std.debug.print("{any}\n", .{v});
}
