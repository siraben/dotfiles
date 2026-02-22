//! Minecraft on-demand listener and proxy.
//!
//! Always owns the public port. When the backend is down, shows a
//! sleeping/starting MOTD. When the backend is up, transparently
//! proxies connections using splice() for zero-copy forwarding.

use std::env;
use std::fs;
use std::io::{self, Read, Write};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::os::fd::AsRawFd;
use std::process::Command;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

const BACKEND_HOST: &str = "127.0.0.1";
const CHECK_INTERVAL: Duration = Duration::from_secs(2);
const MAX_PROXY_CONNS: usize = 100;
const SPLICE_LEN: usize = 65536;
const PKT_MAX: usize = 4096;

static ALIVE: AtomicBool = AtomicBool::new(true);

#[derive(Clone, Copy, PartialEq)]
enum ServerState {
    Sleeping,
    Starting,
    Running,
}

extern "C" fn on_signal(_: libc::c_int) {
    ALIVE.store(false, Ordering::SeqCst);
}

// ---- VarInt ----

fn read_varint(stream: &mut TcpStream) -> io::Result<i32> {
    let mut val: i32 = 0;
    for i in 0..5 {
        let mut b = [0u8; 1];
        stream.read_exact(&mut b)?;
        val |= i32::from(b[0] & 0x7F) << (7 * i);
        if b[0] & 0x80 == 0 {
            return Ok(val);
        }
    }
    Err(io::Error::new(io::ErrorKind::InvalidData, "varint too long"))
}

fn decode_varint(buf: &[u8]) -> Option<(i32, usize)> {
    let mut val: i32 = 0;
    for (i, &b) in buf.iter().enumerate().take(5) {
        val |= i32::from(b & 0x7F) << (7 * i);
        if b & 0x80 == 0 {
            return Some((val, i + 1));
        }
    }
    None
}

fn encode_varint(value: i32) -> Vec<u8> {
    let mut v = value as u32;
    let mut buf = Vec::with_capacity(5);
    loop {
        if v & !0x7F == 0 {
            buf.push(v as u8);
            return buf;
        }
        buf.push((v as u8 & 0x7F) | 0x80);
        v >>= 7;
    }
}

// ---- String ----

fn decode_string(buf: &[u8]) -> Option<(String, usize)> {
    let (len, vi) = decode_varint(buf)?;
    if len < 0 {
        return None;
    }
    let len = len as usize;
    let end = vi.checked_add(len)?;
    let bytes = buf.get(vi..end)?;
    let s = std::str::from_utf8(bytes).ok()?;
    Some((s.to_owned(), end))
}

fn encode_string(s: &str) -> Vec<u8> {
    let mut buf = encode_varint(s.len() as i32);
    buf.extend_from_slice(s.as_bytes());
    buf
}

// ---- Packet ----

fn build_packet(id: i32, data: &[u8]) -> Vec<u8> {
    let id_enc = encode_varint(id);
    let mut pkt = encode_varint((id_enc.len() + data.len()) as i32);
    pkt.extend(&id_enc);
    pkt.extend_from_slice(data);
    pkt
}

// ---- Protocol ----

fn parse_next_state(buf: &[u8]) -> Option<i32> {
    let mut pos = 0;
    let (_, n) = decode_varint(buf.get(pos..)?)?;
    pos = pos.checked_add(n)?;
    let (_, n) = decode_varint(buf.get(pos..)?)?;
    pos = pos.checked_add(n)?;
    let (slen, n) = decode_varint(buf.get(pos..)?)?;
    if slen < 0 {
        return None;
    }
    pos = pos.checked_add(n)?.checked_add(slen as usize)?;
    pos = pos.checked_add(2)?;
    if pos > buf.len() {
        return None;
    }
    let (ns, _) = decode_varint(buf.get(pos..)?)?;
    Some(ns)
}

fn parse_login_name(buf: &[u8]) -> Option<String> {
    let (_, n) = decode_varint(buf)?;
    let (name, _) = decode_string(buf.get(n..)?)?;
    Some(name)
}

// ---- Whitelist ----

fn load_whitelist(path: &str) -> Vec<String> {
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Whitelist file error: {path}: {e}");
            return Vec::new();
        }
    };
    content
        .lines()
        .filter_map(|line| line.trim().strip_prefix("WHITELIST="))
        .flat_map(|val| val.split(','))
        .map(|s| s.trim().to_lowercase())
        .filter(|s| !s.is_empty())
        .collect()
}

fn is_whitelisted(whitelist: &[String], name: &str) -> bool {
    whitelist.is_empty() || whitelist.iter().any(|w| w == &name.to_lowercase())
}

// ---- Handlers ----

fn handle_status(stream: &mut TcpStream, state: ServerState) {
    let motd = if state == ServerState::Starting {
        r#"{"version":{"name":"On-Demand","protocol":-1},"players":{"max":0,"online":0},"description":{"text":"\u00a7eServer is starting up...\n\u00a76Please wait a moment."}}"#
    } else {
        r#"{"version":{"name":"On-Demand","protocol":-1},"players":{"max":0,"online":0},"description":{"text":"\u00a7eServer is sleeping.\n\u00a7aJoin to wake it up!"}}"#
    };
    let pkt = build_packet(0x00, &encode_string(motd));
    let _ = stream.write_all(&pkt);

    if let Ok(len) = read_varint(stream) {
        if len > 0 && (len as usize) <= PKT_MAX {
            let mut buf = vec![0u8; len as usize];
            if stream.read_exact(&mut buf).is_ok()
                && buf.first() == Some(&0x01)
                && buf.len() >= 9
            {
                let _ = stream.write_all(&build_packet(0x01, &buf[1..9]));
            }
        }
    }
}

fn handle_kick(stream: &mut TcpStream, msg: &str) {
    let json = format!(r#"{{"text":"{msg}"}}"#);
    let pkt = build_packet(0x00, &encode_string(&json));
    let _ = stream.write_all(&pkt);
}

// ---- Proxy with zero-copy splice ----

#[cfg(target_os = "linux")]
extern "C" {
    fn splice(
        fd_in: libc::c_int,
        off_in: *mut libc::loff_t,
        fd_out: libc::c_int,
        off_out: *mut libc::loff_t,
        len: libc::size_t,
        flags: libc::c_uint,
    ) -> libc::ssize_t;
}

#[cfg(target_os = "linux")]
const SPLICE_F_MOVE: libc::c_uint = 1;

/// RAII guard for pipe file descriptors.
#[cfg(target_os = "linux")]
struct Pipe(i32, i32);

#[cfg(target_os = "linux")]
impl Pipe {
    fn new() -> io::Result<Self> {
        let mut fds = [0i32; 2];
        if unsafe { libc::pipe(fds.as_mut_ptr()) } < 0 {
            return Err(io::Error::last_os_error());
        }
        Ok(Pipe(fds[0], fds[1]))
    }
}

#[cfg(target_os = "linux")]
impl Drop for Pipe {
    fn drop(&mut self) {
        unsafe {
            libc::close(self.0);
            libc::close(self.1);
        }
    }
}

#[cfg(target_os = "linux")]
fn splice_forward(src: i32, dst: i32) {
    let pipe = match Pipe::new() {
        Ok(p) => p,
        Err(_) => {
            unsafe { libc::shutdown(dst, libc::SHUT_WR) };
            return;
        }
    };
    loop {
        let n = unsafe {
            splice(
                src,
                std::ptr::null_mut(),
                pipe.1,
                std::ptr::null_mut(),
                SPLICE_LEN,
                SPLICE_F_MOVE,
            )
        };
        if n <= 0 {
            break;
        }
        let mut rem = n as usize;
        while rem > 0 {
            let w = unsafe {
                splice(
                    pipe.0,
                    std::ptr::null_mut(),
                    dst,
                    std::ptr::null_mut(),
                    rem,
                    SPLICE_F_MOVE,
                )
            };
            if w <= 0 {
                unsafe { libc::shutdown(dst, libc::SHUT_WR) };
                return;
            }
            rem -= w as usize;
        }
    }
    unsafe { libc::shutdown(dst, libc::SHUT_WR) };
}

#[cfg(not(target_os = "linux"))]
fn splice_forward(src: i32, dst: i32) {
    // Fallback: userspace copy for non-Linux (development only)
    let mut buf = [0u8; SPLICE_LEN];
    loop {
        let n = unsafe { libc::read(src, buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
        if n <= 0 {
            break;
        }
        let mut off = 0;
        while off < n as usize {
            let w = unsafe {
                libc::write(
                    dst,
                    buf[off..].as_ptr() as *const libc::c_void,
                    (n as usize) - off,
                )
            };
            if w <= 0 {
                unsafe { libc::shutdown(dst, libc::SHUT_WR) };
                return;
            }
            off += w as usize;
        }
    }
    unsafe { libc::shutdown(dst, libc::SHUT_WR) };
}

/// RAII guard to decrement connection count.
struct ConnGuard(Arc<AtomicUsize>);

impl Drop for ConnGuard {
    fn drop(&mut self) {
        self.0.fetch_sub(1, Ordering::Relaxed);
    }
}

fn proxy_connection(
    client: TcpStream,
    backend_port: u16,
    initial_data: Vec<u8>,
    conn_count: Arc<AtomicUsize>,
) {
    let _guard = ConnGuard(conn_count);

    let addr: SocketAddr = format!("{BACKEND_HOST}:{backend_port}")
        .parse()
        .unwrap();
    let mut backend = match TcpStream::connect_timeout(&addr, Duration::from_secs(5)) {
        Ok(s) => s,
        Err(_) => return,
    };

    let _ = client.set_nodelay(true);
    let _ = backend.set_nodelay(true);
    let _ = client.set_read_timeout(None);
    let _ = client.set_write_timeout(None);
    let _ = backend.set_read_timeout(None);
    let _ = backend.set_write_timeout(None);

    if backend.write_all(&initial_data).is_err() {
        return;
    }

    let cli_fd = client.as_raw_fd();
    let bck_fd = backend.as_raw_fd();

    let t1 = thread::spawn(move || splice_forward(cli_fd, bck_fd));
    let t2 = thread::spawn(move || splice_forward(bck_fd, cli_fd));

    let _ = t1.join();
    let _ = t2.join();
    // client + backend dropped here (close sockets)
    // _guard dropped here (decrement conn_count)
}

// ---- Backend monitor ----

fn check_backend(port: u16) -> bool {
    let addr: SocketAddr = format!("{BACKEND_HOST}:{port}").parse().unwrap();
    TcpStream::connect_timeout(&addr, Duration::from_secs(1)).is_ok()
}

fn monitor_backend(port: u16, state: Arc<Mutex<ServerState>>) {
    while ALIVE.load(Ordering::Relaxed) {
        let up = check_backend(port);
        {
            let mut s = state.lock().unwrap();
            if *s == ServerState::Starting && up {
                println!("Backend is up");
                *s = ServerState::Running;
            } else if *s == ServerState::Running && !up {
                println!("Backend went down");
                *s = ServerState::Sleeping;
            }
        }
        thread::sleep(CHECK_INTERVAL);
    }
}

// ---- Start server ----

fn start_server() {
    let _ = Command::new("systemctl")
        .args(["start", "--no-block", "podman-minecraft.service"])
        .status();
}

// ---- Main ----

fn main() {
    let args: Vec<String> = env::args().collect();
    let port: u16 = args.get(1).and_then(|s| s.parse().ok()).unwrap_or(25565);
    let backend_port: u16 = args
        .get(2)
        .and_then(|s| s.parse().ok())
        .unwrap_or(port + 1);

    let whitelist = if let Some(path) = args.get(3) {
        let wl = load_whitelist(path);
        if wl.is_empty() {
            println!("Warning: whitelist file given but no players found");
        } else {
            println!("Whitelist loaded: {} players", wl.len());
        }
        wl
    } else {
        Vec::new()
    };

    let state = Arc::new(Mutex::new(ServerState::Sleeping));
    let conn_count = Arc::new(AtomicUsize::new(0));

    unsafe {
        libc::signal(libc::SIGTERM, on_signal as libc::sighandler_t);
        libc::signal(libc::SIGINT, on_signal as libc::sighandler_t);
        libc::signal(libc::SIGPIPE, libc::SIG_IGN);
    }

    let listener = TcpListener::bind(format!("0.0.0.0:{port}")).unwrap_or_else(|e| {
        eprintln!("bind: {e}");
        std::process::exit(1);
    });

    // 1-second accept timeout so we can check ALIVE
    unsafe {
        let tv = libc::timeval {
            tv_sec: 1,
            tv_usec: 0,
        };
        libc::setsockopt(
            listener.as_raw_fd(),
            libc::SOL_SOCKET,
            libc::SO_RCVTIMEO,
            &tv as *const _ as *const libc::c_void,
            std::mem::size_of_val(&tv) as libc::socklen_t,
        );
    }

    {
        let state = state.clone();
        thread::spawn(move || monitor_backend(backend_port, state));
    }

    if check_backend(backend_port) {
        *state.lock().unwrap() = ServerState::Running;
        println!("Backend already up on :{backend_port}");
    }
    println!("Listening on :{port}, backend :{backend_port}");

    while ALIVE.load(Ordering::Relaxed) {
        let mut client = match listener.accept() {
            Ok((s, _)) => s,
            Err(_) => continue,
        };

        let _ = client.set_read_timeout(Some(Duration::from_secs(5)));
        let _ = client.set_write_timeout(Some(Duration::from_secs(5)));

        // Read handshake
        let plen = match read_varint(&mut client) {
            Ok(n) if n > 0 && (n as usize) <= PKT_MAX => n as usize,
            _ => continue,
        };

        let mut hs = vec![0u8; plen];
        if client.read_exact(&mut hs).is_err() || hs.first() != Some(&0x00) {
            continue;
        }

        let ns = match parse_next_state(&hs) {
            Some(n) => n,
            None => continue,
        };

        let mut raw = encode_varint(plen as i32);
        raw.extend_from_slice(&hs);

        let st = *state.lock().unwrap();

        if st == ServerState::Running {
            let _ = client.set_read_timeout(None);
            let _ = client.set_write_timeout(None);

            let prev = conn_count.fetch_add(1, Ordering::Relaxed);
            if prev >= MAX_PROXY_CONNS {
                conn_count.fetch_sub(1, Ordering::Relaxed);
                continue;
            }

            let cc = conn_count.clone();
            thread::spawn(move || proxy_connection(client, backend_port, raw, cc));
            continue;
        }

        if ns == 1 {
            // Status request
            if let Ok(srl) = read_varint(&mut client) {
                if srl > 0 && (srl as usize) <= PKT_MAX {
                    let mut tmp = vec![0u8; srl as usize];
                    let _ = client.read_exact(&mut tmp);
                }
            }
            handle_status(&mut client, st);
        } else if ns == 2 {
            // Login
            let username = (|| -> Option<String> {
                let ll = read_varint(&mut client).ok()?;
                if ll <= 0 || ll as usize > PKT_MAX {
                    return None;
                }
                let mut ld = vec![0u8; ll as usize];
                client.read_exact(&mut ld).ok()?;
                parse_login_name(&ld)
            })();

            let name_str = username.as_deref().unwrap_or("(unknown)");

            if !whitelist.is_empty()
                && !username
                    .as_ref()
                    .map_or(false, |n| is_whitelisted(&whitelist, n))
            {
                println!("Rejected non-whitelisted user: {name_str}");
                handle_kick(&mut client, "You are not whitelisted on this server.");
            } else {
                handle_kick(
                    &mut client,
                    "Server is starting, please reconnect in ~30 seconds.",
                );
                let should_start = {
                    let mut s = state.lock().unwrap();
                    if *s == ServerState::Sleeping {
                        *s = ServerState::Starting;
                        true
                    } else {
                        false
                    }
                };
                if should_start {
                    println!("Login from {name_str}, starting server...");
                    start_server();
                }
            }
        }
    }

    println!("Shutting down");
}
