#!/usr/bin/env python3
"""Minecraft on-demand listener and proxy.

Always owns the public port. When the backend is down, shows a
sleeping/starting MOTD. When the backend is up, transparently
proxies connections. Never gives "can't connect".
"""

import json
import signal
import socket
import struct
import subprocess
import sys
import threading
import time

BACKEND_HOST = "127.0.0.1"
CHECK_INTERVAL = 2

SLEEPING = "sleeping"
STARTING = "starting"
RUNNING = "running"

state = SLEEPING
lock = threading.Lock()
alive = True


# -- Minecraft protocol helpers --

def read_varint(sock):
    result = 0
    for i in range(5):
        byte = sock.recv(1)
        if not byte:
            raise ConnectionError
        b = byte[0]
        result |= (b & 0x7F) << (7 * i)
        if not (b & 0x80):
            return result
    return result


def encode_varint(value):
    buf = b""
    while True:
        if (value & ~0x7F) == 0:
            return buf + struct.pack("B", value)
        buf += struct.pack("B", (value & 0x7F) | 0x80)
        value >>= 7


def encode_string(s):
    data = s.encode("utf-8")
    return encode_varint(len(data)) + data


def make_packet(packet_id, data):
    body = encode_varint(packet_id) + data
    return encode_varint(len(body)) + body


def parse_next_state(data):
    """Extract next_state from a handshake packet."""
    pos = 0
    # skip packet id varint
    while data[pos] & 0x80:
        pos += 1
    pos += 1
    # skip protocol version varint
    while data[pos] & 0x80:
        pos += 1
    pos += 1
    # skip server address string
    str_len, shift = 0, 0
    while data[pos] & 0x80:
        str_len |= (data[pos] & 0x7F) << shift
        shift += 7
        pos += 1
    str_len |= (data[pos] & 0x7F) << shift
    pos += 1 + str_len
    # skip server port
    pos += 2
    # next_state varint
    ns, shift = 0, 0
    while pos < len(data) and data[pos] & 0x80:
        ns |= (data[pos] & 0x7F) << shift
        shift += 7
        pos += 1
    if pos < len(data):
        ns |= (data[pos] & 0x7F) << shift
    return ns


# -- Connection handlers --

def handle_status(client):
    with lock:
        s = state
    if s == STARTING:
        motd = "\u00a7eServer is starting up...\n\u00a76Please wait a moment."
    else:
        motd = "\u00a7eServer is sleeping.\n\u00a7aJoin to wake it up!"

    resp = json.dumps({
        "version": {"name": "On-Demand", "protocol": -1},
        "players": {"max": 0, "online": 0},
        "description": {"text": motd},
    })
    client.sendall(make_packet(0x00, encode_string(resp)))

    try:
        length = read_varint(client)
        data = client.recv(length)
        if data and data[0] == 0x01 and len(data) >= 9:
            client.sendall(make_packet(0x01, data[1:9]))
    except Exception:
        pass


def handle_login_kick(client):
    msg = json.dumps({"text": "Server is starting, please reconnect in ~30 seconds."})
    client.sendall(make_packet(0x00, encode_string(msg)))


def proxy(client, backend_port, initial_data):
    """Proxy a connection to the backend server."""
    backend = None
    try:
        backend = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        backend.settimeout(5)
        backend.connect((BACKEND_HOST, backend_port))
        backend.settimeout(None)
        backend.sendall(initial_data)

        def forward(src, dst):
            try:
                while True:
                    data = src.recv(8192)
                    if not data:
                        break
                    dst.sendall(data)
            except Exception:
                pass
            try:
                dst.shutdown(socket.SHUT_WR)
            except Exception:
                pass

        t1 = threading.Thread(target=forward, args=(client, backend), daemon=True)
        t2 = threading.Thread(target=forward, args=(backend, client), daemon=True)
        t1.start()
        t2.start()
        t1.join()
        t2.join()
    except Exception:
        pass
    finally:
        for s in (client, backend):
            if s:
                try:
                    s.close()
                except Exception:
                    pass


# -- Backend monitor --

def check_backend(backend_port):
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(1)
        s.connect((BACKEND_HOST, backend_port))
        s.close()
        return True
    except Exception:
        return False


def monitor_backend(backend_port):
    global state
    while alive:
        up = check_backend(backend_port)
        with lock:
            if state == STARTING and up:
                print("Backend is up")
                state = RUNNING
            elif state == RUNNING and not up:
                print("Backend went down")
                state = SLEEPING
        time.sleep(CHECK_INTERVAL)


# -- Main --

def main():
    global state, alive

    port = int(sys.argv[1]) if len(sys.argv) > 1 else 25565
    backend_port = int(sys.argv[2]) if len(sys.argv) > 2 else port + 1

    srv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    srv.bind(("", port))
    srv.listen(16)
    srv.settimeout(1.0)

    def shutdown(*_):
        global alive
        alive = False

    signal.signal(signal.SIGTERM, shutdown)

    threading.Thread(target=monitor_backend, args=(backend_port,), daemon=True).start()

    if check_backend(backend_port):
        state = RUNNING
        print(f"Backend already up on :{backend_port}")

    print(f"Listening on :{port}, backend :{backend_port}")

    while alive:
        try:
            client, addr = srv.accept()
        except socket.timeout:
            continue
        except OSError:
            break

        try:
            client.settimeout(5)
            length = read_varint(client)
            handshake = client.recv(length)

            if not handshake or handshake[0] != 0x00:
                client.close()
                continue

            next_state = parse_next_state(handshake)
            raw = encode_varint(length) + handshake

            with lock:
                s = state

            if s == RUNNING:
                client.settimeout(None)
                threading.Thread(
                    target=proxy, args=(client, backend_port, raw), daemon=True
                ).start()
                continue

            # sleeping or starting
            if next_state == 1:
                try:
                    ln = read_varint(client)
                    client.recv(ln)
                except Exception:
                    pass
                handle_status(client)
            elif next_state == 2:
                handle_login_kick(client)
                with lock:
                    if state == SLEEPING:
                        state = STARTING
                        print("Login attempt, starting server...")
                        subprocess.run(
                            ["systemctl", "start", "--no-block",
                             "podman-minecraft.service"]
                        )
            client.close()
        except Exception:
            try:
                client.close()
            except Exception:
                pass

    srv.close()
    print("Shutting down")


if __name__ == "__main__":
    main()
