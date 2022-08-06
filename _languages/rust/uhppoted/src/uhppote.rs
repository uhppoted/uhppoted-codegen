#[path = "encode.rs"]
mod encode;

pub fn get_all_controllers() -> Result<i32, String> {
    let request = encode::get_controller_request(0);

    match request {
        Ok(rq) => {
            dump(&rq);
            return Ok(1);
        }

        Err(error) => return Err(error),
    }
}

pub fn get_controller(device_id: u32) -> Result<i32, String> {
    let request = encode::get_controller_request(device_id);

    match request {
        Ok(rq) => {
            dump(&rq);
            return Ok(2);
        }

        Err(error) => return Err(error),
    }
}

pub fn dump(packet: &[u8; 64]) {
    for i in 0..4 {
        let offset = i * 16;
        let u = &packet[offset..offset + 8];
        let v = &packet[offset + 8..offset + 16];

        let p = format!(
            "{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}",
            u[0], u[1], u[2], u[3], u[4], u[5], u[6], u[7]
        );
        let q = format!(
            "{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}",
            v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]
        );

        println!("   {:08x}  {}  {}", offset, p, q);
    }
}
