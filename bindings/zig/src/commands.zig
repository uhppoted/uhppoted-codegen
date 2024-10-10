const std = @import("std");

const uhppote = @import("uhppote/uhppote.zig");
const decode = @import("uhppote/decode.zig");
const datelib = @import("uhppote/datetime.zig");
const network = @import("uhppote/network.zig");

const CONTROLLER: u32 = 405419896;
const DOOR: u8 = 3;
const MODE: u8 = 2;
const DELAY: u8 = 10;
const CARD: u32 = 8165538;
const CARD_INDEX: u32 = 3;
const EVENT_INDEX: u32 = 37;
const TIME_PROFILE_ID: u8 = 29;

var CONTROLLERS = std.AutoHashMapUnmanaged(u32, uhppote.Controller){};

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

    Command{
        .name = "set-ip",
        .function = set_ip,
    },

    Command{
        .name = "get-time",
        .function = get_time,
    },

    Command{
        .name = "set-time",
        .function = set_time,
    },

    Command{
        .name = "get-status",
        .function = get_status,
    },

    Command{
        .name = "get-listener",
        .function = get_listener,
    },

    Command{
        .name = "set-listener",
        .function = set_listener,
    },

    Command{
        .name = "get-door-control",
        .function = get_door_control,
    },

    Command{
        .name = "set-door-control",
        .function = set_door_control,
    },

    Command{
        .name = "open-door",
        .function = open_door,
    },

    Command{
        .name = "get-cards",
        .function = get_cards,
    },

    Command{
        .name = "get-card",
        .function = get_card,
    },

    Command{
        .name = "get-card-by-index",
        .function = get_card_by_index,
    },

    Command{
        .name = "put-card",
        .function = put_card,
    },

    Command{
        .name = "delete-card",
        .function = delete_card,
    },

    Command{
        .name = "delete-all-cards",
        .function = delete_all_cards,
    },

    Command{
        .name = "get-event",
        .function = get_event,
    },

    Command{
        .name = "get-event-index",
        .function = get_event_index,
    },

    Command{
        .name = "set-event-index",
        .function = set_event_index,
    },

    Command{
        .name = "record-special-events",
        .function = record_special_events,
    },

    Command{
        .name = "get-time-profile",
        .function = get_time_profile,
    },

    Command{
        .name = "set-time-profile",
        .function = set_time_profile,
    },

    Command{
        .name = "delete-all-time-profiles",
        .function = delete_all_time_profiles,
    },

    Command{
        .name = "add-task",
        .function = add_task,
    },

    Command{
        .name = "refresh-tasklist",
        .function = refresh_tasklist,
    },

    Command{
        .name = "clear-tasklist",
        .function = clear_tasklist,
    },

    Command{
        .name = "set-pc-control",
        .function = set_pc_control,
    },

    Command{
        .name = "set-interlock",
        .function = set_interlock,
    },

    Command{
        .name = "activate-keypads",
        .function = activate_keypads,
    },

    Command{
        .name = "set-door-passcodes",
        .function = set_door_passcodes,
    },

    Command{
        .name = "restore-default-parameters",
        .function = restore_default_parameters,
    },

    Command{
        .name = "listen",
        .function = listen,
    },
};

pub fn init(allocator: std.mem.Allocator) !void {
    try CONTROLLERS.put(allocator,405419896, uhppote.Controller {
        .controller = 405419896,
        .address = "192.168.1.100:60000",
        .transport = "tcp",
    });

    try CONTROLLERS.put(allocator,303986753, uhppote.Controller {
        .controller = 303986753,
        .address = "192.168.1.100:60000",
        .transport = "udp",
    });
}

pub fn deinit(allocator: std.mem.Allocator) void {
    CONTROLLERS.deinit(allocator);
}

pub fn exec(cmd: Command) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    cmd.function(allocator);
}

fn get_all_controllers(allocator: std.mem.Allocator) void {
    const list = uhppote.get_all_controllers(allocator);
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
    const controller = resolve(CONTROLLER);

    if (uhppote.get_controller(controller, allocator)) |c| {
        pprint(c);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_ip(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const address = network.Address.IPv4.init(192, 168, 1, 100);
    const netmask = network.Address.IPv4.init(255, 255, 255, 0);
    const gateway = network.Address.IPv4.init(192, 168, 1, 1);

    if (uhppote.set_ip(controller, address, netmask, gateway, allocator)) |ok| {
        pprint(ok);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_time(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);

    if (uhppote.get_time(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_time(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const now = datelib.now();

    if (uhppote.set_time(controller, now, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_status(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);

    if (uhppote.get_status(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_listener(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);

    if (uhppote.get_listener(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_listener(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const address = network.Address.IPv4.init(192, 168, 1, 100);
    const port = 60002;
    const interval = 15;

    if (uhppote.set_listener(controller, address, port, interval, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_door_control(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const door = DOOR;

    if (uhppote.get_door_control(controller, door, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_door_control(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
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
    const controller = resolve(CONTROLLER);
    const door = DOOR;

    if (uhppote.open_door(controller, door, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_cards(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);

    if (uhppote.get_cards(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_card(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const card = CARD;

    if (uhppote.get_card(controller, card, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_card_by_index(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const index = CARD_INDEX;

    if (uhppote.get_card_by_index(controller, index, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn put_card(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const card = CARD;
    const start = datelib.Date{ .year = 2023, .month = 1, .day = 1 };
    const end = datelib.Date{ .year = 2023, .month = 12, .day = 31 };
    const door1 = 0;
    const door2 = 1;
    const door3 = 29;
    const door4 = 0;
    const pin = 7531;

    if (uhppote.put_card(controller, card, start, end, door1, door2, door3, door4, pin, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn delete_card(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const card = CARD;

    if (uhppote.delete_card(controller, card, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn delete_all_cards(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);

    if (uhppote.delete_all_cards(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_event(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const index = EVENT_INDEX;

    if (uhppote.get_event(controller, index, allocator)) |response| {
        if (response.event_type == 0xff) {
            std.debug.print("\n   *** ERROR  event @ index {any} overwritten\n", .{index});
        } else if (response.index == 0) {
            std.debug.print("\n   *** ERROR  event @ index {any} not found\n", .{index});
        } else {
            pprint(response);
        }
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_event_index(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);

    if (uhppote.get_event_index(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_event_index(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const index = EVENT_INDEX;

    if (uhppote.set_event_index(controller, index, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn record_special_events(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const enabled = true;

    if (uhppote.record_special_events(controller, enabled, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn get_time_profile(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const profile_id = TIME_PROFILE_ID;

    if (uhppote.get_time_profile(controller, profile_id, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_time_profile(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const profile_id = TIME_PROFILE_ID;
    const start = datelib.Date{ .year = 2023, .month = 1, .day = 1 };
    const end = datelib.Date{ .year = 2023, .month = 12, .day = 31 };
    const monday = true;
    const tuesday = true;
    const wednesday = false;
    const thursday = true;
    const friday = false;
    const saturday = false;
    const sunday = true;
    const segment_1_start = datelib.Time{ .hour = 8, .minute = 30, .second = 0 };
    const segment_1_end = datelib.Time{ .hour = 11, .minute = 45, .second = 0 };
    const segment_2_start = datelib.Time{ .hour = 12, .minute = 45, .second = 0 };
    const segment_2_end = datelib.Time{ .hour = 17, .minute = 0, .second = 0 };
    const segment_3_start = datelib.Time{ .hour = 18, .minute = 30, .second = 0 };
    const segment_3_end = datelib.Time{ .hour = 20, .minute = 15, .second = 0 };
    const linked_profile_id = 37;

    if (uhppote.set_time_profile(controller, profile_id, start, end, monday, tuesday, wednesday, thursday, friday, saturday, sunday, segment_1_start, segment_1_end, segment_2_start, segment_2_end, segment_3_start, segment_3_end, linked_profile_id, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn delete_all_time_profiles(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);

    if (uhppote.delete_all_time_profiles(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn add_task(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const start_date = datelib.Date{ .year = 2023, .month = 1, .day = 1 };
    const end_date = datelib.Date{ .year = 2023, .month = 12, .day = 31 };
    const monday = true;
    const tuesday = true;
    const wednesday = false;
    const thursday = true;
    const friday = false;
    const saturday = false;
    const sunday = true;
    const start_time = datelib.Time{ .hour = 8, .minute = 30, .second = 0 };
    const door = DOOR;
    const task_type = 2;
    const more_cards = 0;

    if (uhppote.add_task(controller, start_date, end_date, monday, tuesday, wednesday, thursday, friday, saturday, sunday, start_time, door, task_type, more_cards, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn refresh_tasklist(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);

    if (uhppote.refresh_tasklist(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn clear_tasklist(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);

    if (uhppote.clear_tasklist(controller, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_pc_control(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const enabled = true;

    if (uhppote.set_pc_control(controller, enabled, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_interlock(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const interlock = 3;

    if (uhppote.set_interlock(controller, interlock, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn activate_keypads(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const reader1 = true;
    const reader2 = true;
    const reader3 = false;
    const reader4 = true;

    if (uhppote.activate_keypads(controller, reader1, reader2, reader3, reader4, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn set_door_passcodes(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);
    const door = DOOR;
    const passcode1 = 12345;
    const passcode2 = 0;
    const passcode3 = 999999;
    const passcode4 = 54321;

    if (uhppote.set_door_passcodes(controller, door, passcode1, passcode2, passcode3, passcode4, allocator)) |response| {
        pprint(response);
    } else |err| {
        std.debug.print("\n   *** ERROR  {any}\n", .{err});
    }
}

fn restore_default_parameters(allocator: std.mem.Allocator) void {
    const controller = resolve(CONTROLLER);

    if (uhppote.restore_default_parameters(controller, allocator)) |response| {
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

fn resolve(controller: u32) uhppote.Controller {
    return CONTROLLERS.get(controller) orelse uhppote.Controller {
        .controller = controller,
        .address = "",
        .transport = "udp",
    };
}

fn pprint(v: anytype) void {
    std.debug.print("{any}\n", .{v});
}
