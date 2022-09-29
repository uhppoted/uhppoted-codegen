use std::net::UdpSocket;
use std::sync::RwLock;
use std::time::Duration;

use async_std::future;
use futures;
use futures::future::FutureExt;
use lazy_static::lazy_static;

use super::error;

pub type Msg = [u8; 64];
pub type Result<T> = std::result::Result<T, error::Error>;

const READ_TIMEOUT: Duration = Duration::from_millis(5000);
const WRITE_TIMEOUT: Duration = Duration::from_millis(1000);
const TIMEOUT: Duration = Duration::from_millis(2500);

lazy_static! {
    static ref BIND_ADDR: RwLock<String> = RwLock::new("0.0.0.0:0".to_string());
    static ref BROADCAST_ADDR: RwLock<String> = RwLock::new("255.255.255.255:60000".to_string());
    static ref LISTEN_ADDR: RwLock<String> = RwLock::new("0.0.0.0:60001".to_string());
    static ref DEBUG: RwLock<bool> = RwLock::new(false);
}

pub fn set_bind_addr(addr: &str) {
    if let Ok(mut guard) = BIND_ADDR.write() {
        *guard = addr.to_string() + ":0";
    }
}

pub fn set_broadcast_addr(addr: &str) {
    if let Ok(mut guard) = BROADCAST_ADDR.write() {
        *guard = addr.to_string();
    }
}

pub fn set_listen_addr(addr: &str) {
    if let Ok(mut guard) = LISTEN_ADDR.write() {
        *guard = addr.to_string();
    }
}

pub fn set_debug(enabled: bool) {
    if let Ok(mut guard) = DEBUG.write() {
        *guard = enabled;
    }
}

pub fn broadcast(packet: &Msg) -> Result<Vec<Msg>> {
    let bind = BIND_ADDR.read()?;
    let broadcast = BROADCAST_ADDR.read()?;
    let socket = UdpSocket::bind(bind.as_str())?;

    dump(&packet);

    socket.set_write_timeout(Some(WRITE_TIMEOUT))?;
    socket.set_read_timeout(Some(READ_TIMEOUT))?;
    socket.set_broadcast(true)?;
    socket.send_to(packet, broadcast.as_str())?;

    let g = read_all(socket);

    match futures::executor::block_on(g) {
        Ok(replies) => return Ok(replies),
        Err(e) => return Err(e),
    }
}

pub fn send(packet: &Msg) -> Result<Option<Msg>> {
    let bind = BIND_ADDR.read()?;
    let broadcast = BROADCAST_ADDR.read()?;
    let socket = UdpSocket::bind(bind.as_str())?;

    dump(&packet);

    socket.set_write_timeout(Some(WRITE_TIMEOUT))?;
    socket.set_read_timeout(Some(READ_TIMEOUT))?;
    socket.set_broadcast(true)?;
    socket.send_to(packet, broadcast.as_str())?;

    if packet[1] == 0x96 {
        return Ok(None);
    }

    let g = read(socket);

    match futures::executor::block_on(g) {
        Ok(reply) => Ok(Some(reply)),
        Err(e) => return Err(e),
    }
}

async fn read(socket: UdpSocket) -> Result<Msg> {
    let f = future::timeout(TIMEOUT, async {
        let sock = async_std::net::UdpSocket::from(socket);
        let mut buffer = [0u8; 1024];

        let result: Result<Msg> = loop {
            match sock.recv(&mut buffer).await {
                Ok(64) => {
                    dump(buffer[..64].try_into()?);
                    break Ok(buffer[..64].try_into()?);
                }

                Err(e) => break Err(error::Error::from(e)),
                _ => continue,
            }
        };

        return result;
    });

    return async {
        match f.await {
            Ok(v) => match v {
                Ok(reply) => return Ok(reply),
                Err(e) => return Err(e),
            },

            Err(e) => return Err(error::Error::from(e)),
        }
    }.await;
}

async fn read_all(socket: UdpSocket) -> Result<Vec<Msg>> {
    let replies = RwLock::<Vec<Msg>>::new(vec![]);

    let f = future::timeout(Duration::from_millis(2500), async {
        let sock = async_std::net::UdpSocket::from(socket);
        let mut buffer = [0u8; 1024];

        let result: Result<()> = loop {
            match sock.recv(&mut buffer).await {
                Ok(64) => {
                    dump(buffer[..64].try_into()?);
                    replies.write().unwrap().push(buffer[..64].try_into()?);
                }

                Err(e) => break Err(error::Error::from(e)),
                _ => continue,
            }
        };

        return result;
    });

    return async {
        match f.await {
            Ok(v) => match v {
                Ok(_) => return Ok(replies.read().unwrap().to_vec()),
                Err(e) => return Err(error::Error::from(e)),
            },

            Err(_) => return Ok(replies.read().unwrap().to_vec()),
        }
    }.await;
}

//TODO should probably use a stream/channel
pub fn listen(events: impl Fn(Msg), errors: impl Fn(error::Error), interrupt: impl future::Future) -> Result<()> {
    let bind = LISTEN_ADDR.read()?;
    let socket = UdpSocket::bind(bind.as_str())?;

    socket.set_read_timeout(None)?;

    let f = async {
        let sock = async_std::net::UdpSocket::from(socket);
        let mut buffer = [0u8; 1024];

        loop {
            match sock.recv(&mut buffer).await {
                Ok(64) => {
                    let mut packet = [0u8; 64];

                    packet.clone_from_slice(&buffer[..64]);
                    dump(&packet);
                    events(packet);
                }

                Err(e) => errors(error::Error::from(e)),

                _ => continue,
            }
        }
    }.fuse();

    let interrupted = interrupt.fuse();

    futures::pin_mut!(f, interrupted);

    let g = async { 
        futures::select! {
            _ = f => {}
            _ = interrupted => {
            },
        }
    };

    futures::executor::block_on(g);

    return Ok(());
}

fn dump(packet: &Msg) {
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
