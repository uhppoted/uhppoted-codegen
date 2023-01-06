const std = @import("std");
const network = @import("zig-network");

const uhppote = @import("uhppote/uhppote.zig");
const decode = @import("uhppote/decode.zig");
const datelib = @import("uhppote/datetime.zig");

const CONTROLLER: u32 = 405419896;
const DOOR: u8 = 3;
const MODE: u8 = 2;
const DELAY: u8 = 10;
const CARD: u32 = 8165538;
const CARD_INDEX: u32 = 3;
// const EVENT_INDEX: u32 = 37;
// const TIME_PROFILE_ID: u8 = 29;

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

    Command {
        .name = "get-time",
        .function = get_time,
    },

    Command {
        .name = "set-time",
        .function = set_time,
    },

   Command {
       .name = "get-status",
       .function = get_status,
   },

   Command {
       .name = "get-listener",
       .function = get_listener,
   },

   Command {
       .name = "set-listener",
       .function = set_listener,
   },

   Command {
       .name = "get-door-control",
       .function = get_door_control,
   },

   Command {
       .name = "set-door-control",
       .function = set_door_control,
   },

   Command {
       .name = "open-door",
       .function = open_door,
   },

   Command {
       .name = "get-cards",
       .function = get_cards,
   },

   Command {
       .name = "get-card",
       .function = get_card,
   },

   Command {
       .name = "get-card-by-index",
       .function = get_card_by_index,
   },

   Command {
       .name = "put-card",
       .function = put_card,
   },

   Command {
       .name = "delete-card",
       .function = delete_card,
   },

   Command {
       .name = "delete-all-cards",
       .function = delete_all_cards,
   },

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

fn get_time(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;

    if (uhppote.get_time(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_time(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;
    const now = datelib.now();

    if (uhppote.set_time(controller, now, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_status(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;

    if (uhppote.get_status(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_listener(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;

    if (uhppote.get_listener(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_listener(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;
    const address = network.Address.IPv4.init(192,168,1,100);
    const port = 60002;

    if (uhppote.set_listener(controller, address, port, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_door_control(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;
    const door = DOOR;

    if (uhppote.get_door_control(controller, door, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_door_control(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;
    const door = DOOR;
    const mode = MODE;
    const delay = DELAY;

    if (uhppote.set_door_control(controller, door, mode, delay, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn open_door(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;
    const door = DOOR;

    if (uhppote.open_door(controller, door, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_cards(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;

    if (uhppote.get_cards(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_card(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;
    const card = CARD;

    if (uhppote.get_card(controller, card, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_card_by_index(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;
    const index = CARD_INDEX;

    if (uhppote.get_card_by_index(controller, index, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn put_card(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;
    const card = CARD;
    const start = datelib.Date{.year=2023, .month=1, .day=1};
    const end = datelib.Date{.year=2023, .month=12, .day=31};
    const door1 = 0;
    const door2 = 1;
    const door3 = 29;
    const door4 = 0;

    if (uhppote.put_card(controller, card, start, end, door1, door2, door3, door4, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn delete_card(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;
    const card = CARD;

    if (uhppote.delete_card(controller, card, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn delete_all_cards(allocator: std.mem.Allocator) void {
    const controller = CONTROLLER;

    if (uhppote.delete_all_cards(controller, allocator)) |response| {
        pprint(response);
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
