use std::any::Any;
use std::io;
use std::error::Error;
use std::net::UdpSocket;
use std::time::Duration;

#[path = "encode.rs"]
mod encode;

#[path = "decode.rs"]
mod decode;

const READ_TIMEOUT: Duration = Duration::from_millis(5000);
const WRITE_TIMEOUT: Duration = Duration::from_millis(1000);

// static bindAddr: String = "0.0.0.0:0".to_string();
// static destAddr: String = "192.168.1.255:60000".to_string();

pub fn get_all_controllers() -> Result<Box<dyn Any>, Box<dyn Error>> {
    let request = encode::get_controller_request(0)?;

    dump(&request);

    for reply in send(&request)? {
        dump(&reply);
        let response = decode::get_controller_response(&reply)?;

        println!(">>> {:?}",response);

        return Ok(Box::new(response));
    }

    return Ok(Box::new(1));
}

pub fn get_controller(device_id: u32) -> Result<Box<dyn Any>, Box<dyn Error>> {
    let request = encode::get_controller_request(device_id)?;

    dump(&request);

    for reply in send(&request)? {
        dump(&reply);
        
        let response = decode::get_controller_response(&reply)?;

        println!(">>> {:?}",response);

        return Ok(Box::new(response));
    }

    return Ok(Box::new(2));
}

pub fn send(packet: &[u8; 64]) -> Result<Vec<[u8; 64]>, io::Error> {
    let socket = UdpSocket::bind("0.0.0.0:60001")?;

    socket.set_write_timeout(Some(WRITE_TIMEOUT))?;
    socket.set_read_timeout(Some(READ_TIMEOUT))?;
    socket.send_to(packet, "192.168.1.100:60000")?;

    let mut replies: Vec<[u8; 64]> = vec![];
    let mut buffer = [0x00; 1024];

    match socket.recv(&mut buffer) {
        Ok(n) => {
            if n == 64 {
                let mut reply = [0x00; 64];

                reply.clone_from_slice(&buffer[..n]);
                replies.push(reply);
            }
        }

        Err(e) => println!("recv function failed: {e:?}"),
    }

    return Ok(replies);
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

        println!("   {offset:08x}  {p}  {q}");
    }

    println!();
}
