use std::error::Error;
use std::net::UdpSocket;
use std::sync::RwLock;
use std::time::Duration;

use async_std::future;
use futures;
use futures::future::FutureExt;
use lazy_static::lazy_static;

use super::errors;

const READ_TIMEOUT: Duration = Duration::from_millis(5000);
const WRITE_TIMEOUT: Duration = Duration::from_millis(1000);
const TIMEOUT: Duration = Duration::from_millis(2500);

lazy_static! {
    static ref BIND_ADDR: RwLock<String> = RwLock::new("0.0.0.0:0".to_string());
    static ref BROADCAST_ADDR: RwLock<String> = RwLock::new("255.255.255.255:60000".to_string());
    static ref DEBUG: RwLock<bool> = RwLock::new(false);
}

pub fn set_bind_addr(addr: &str) {
    if let Ok(mut guard) = BIND_ADDR.write() {
        *guard = addr.to_string();
    }
}

pub fn set_broadcast_addr(addr: &str) {
    if let Ok(mut guard) = BROADCAST_ADDR.write() {
        *guard = addr.to_string();
    }
}

pub fn set_debug(enabled: bool) {
    if let Ok(mut guard) = DEBUG.write() {
        *guard = enabled;
    }
}

pub fn send(packet: &[u8; 64], wait: Duration) -> Result<Vec<[u8; 64]>, Box<dyn Error>> {
    let bind = BIND_ADDR.read()?;
    let broadcast = BROADCAST_ADDR.read()?;
    let socket = UdpSocket::bind(bind.as_str())?;

    dump(&packet);

    socket.set_write_timeout(Some(WRITE_TIMEOUT))?;
    socket.set_read_timeout(Some(READ_TIMEOUT))?;
    socket.set_broadcast(true)?;
    socket.send_to(packet, broadcast.as_str())?;

    match futures::executor::block_on(recv(socket, wait)) {
        Ok(replies) => return Ok(replies),
        Err(e) => return Err(e),
    }
}

async fn recv(socket: UdpSocket, wait: Duration) -> Result<Vec<[u8; 64]>, Box<dyn Error>> {
    let replies = RwLock::<Vec<[u8; 64]>>::new(vec![]);

    let read = async {
        let sock = async_std::net::UdpSocket::from(socket);
        let mut buffer = [0u8; 1024];

        loop {
            match sock.recv(&mut buffer).await {
                Ok(64) => {
                    let mut reply = [0u8; 64];
                    reply.clone_from_slice(&buffer[..64]);
                    replies.write().unwrap().push(reply);

                    dump(&reply);

                    if wait.is_zero() {
                        return Ok(());
                    }
                }

                Err(e) => return Err(e),

                _ => continue,
            }
        }
    }
    .fuse();

    let waited = future::timeout(wait, future::pending::<()>()).fuse();
    let timeout = future::timeout(wait.saturating_add(TIMEOUT), future::pending::<()>()).fuse();

    futures::pin_mut!(waited, read, timeout);

    loop {
        futures::select! {
            v = read => {
                match v {
                    Ok(_) =>  return Ok(replies.read().unwrap().to_vec()),
                    Err(e) => return Err(Box::new(e))
                }
            },

            _ = waited => {
                if !wait.is_zero() {
                    return Ok(replies.read().unwrap().to_vec())
                }
            },

            _ = timeout => {
                return Err(Box::new(errors::Timeout));
            }
        }
    }
}

fn dump(packet: &[u8; 64]) {
    let debug = if let Ok(guard) = DEBUG.read() {
        *guard
    } else {
        false
    };

    if debug {
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
}
