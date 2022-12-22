const std = @import("std");
const network = @import("zig-network");

var debug: bool = false;

pub fn set_debug(v: bool) void {
    debug = v;
}

pub fn broadcast(packet: [64]u8) !void {
    var socket = try network.Socket.create(.ipv4, .udp);

    defer socket.close();

    dump(packet);

    const bindAddr = network.EndPoint{
        .address = network.Address{ .ipv4 = network.Address.IPv4.any },
        .port = 0,
    };

    const broadcastAddr = network.EndPoint{
        .address = network.Address{ .ipv4 = network.Address.IPv4.init(192, 168, 1, 100) },
        .port = 60000,
    };

    try socket.bind(bindAddr);
    const N = try socket.sendTo(broadcastAddr, &packet);

    std.debug.print(">>> SENT: {any}\n", .{N});

    //    const buflen: usize = 128;
    //    var msg: [buflen]u8 = undefined;
    //
    //    while (true) {
    //        const recv_msg = try sock.receiveFrom(msg[0..buflen]);
    //        var last_char = recv_msg.numberOfBytes;
    //
    //        // dirty hack to trim any trailing CR/LF from the input
    //        // could use std.mem.trimRight twice ?
    //        if (msg[last_char - 1] == 10) {
    //            last_char -= 1;
    //        }
    //        if (msg[last_char - 1] == 13) {
    //            last_char -= 1;
    //        }
    //
    //        var name = msg[0..last_char];
    //
    //        try server_list.append(Server{
    //            .address = recv_msg.sender.address,
    //            .name = try allocator.dupe(u8, name),
    //        });
    //    }

}

// pub async n broadcast(packet: &Msg) -> Result<Vec<Msg>> {
//     let bind = BIND_ADDR.read()?;
//     let broadcast = BROADCAST_ADDR.read()?;
//     let socket = UdpSocket::bind(bind.as_str())?;
//
//     dump(&packet);
//
//     socket.set_write_timeout(Some(WRITE_TIMEOUT))?;
//     socket.set_read_timeout(Some(READ_TIMEOUT))?;
//     socket.set_broadcast(true)?;
//     socket.send_to(packet, broadcast.as_str())?;
//
//     return read_all(socket).await;
// }

fn dump(packet: [64]u8) void {
    if (debug) {
        const offsets = [_]usize{ 0, 16, 32, 48 };
        for (offsets) |ix| {
            const f = "{x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2}";
            const u = packet[ix .. ix + 8];
            const v = packet[ix + 8 .. ix + 16];
            std.debug.print(f, .{ u[0], u[1], u[2], u[3], u[4], u[5], u[6], u[7] });
            std.debug.print("  ", .{});
            std.debug.print(f, .{ v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7] });
            std.debug.print("\n", .{});
        }

        std.debug.print("\n", .{});
    }
}
