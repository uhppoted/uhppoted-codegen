const std = @import("std");

const RQ = [64]u8{
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
};

pub fn get_controller_request(device_id: u32) ![64]u8 {
    std.debug.print("get-controller-request {any}\n", .{device_id});

    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator  = &gpa_instance.allocator;
    //
    // const packet = try gpa.alloc(u8, 64);

    if (device_id == 123) {
        return error.Ooops;
    } else {
        return RQ;
    }
}

// pub fn get_controller_request(device_id: u32) -> Result<Msg> {
//     let mut packet = [0x00; 64];
//
//     packet[0] = 0x17;
//     packet[1] = 0x94;
//
//     pack_uint32(device_id, &mut packet, 4)?;
//
//     return Ok(packet);
// }

