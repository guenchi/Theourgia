"""Bounded byte transport and Unix listener lifecycle; no verb semantics."""
from __future__ import annotations
import asyncio
import errno
import fcntl
import os
from pathlib import Path
import re
import selectors
import signal
import socket
import stat
import subprocess
import sys
import time

FRAME_LIMIT = 1048576
ANSWER_LIMIT = 33554432
CONNECT_SECONDS = 0.5
PEER_SECONDS = 5
DRAIN_SECONDS = 5

class ExchangeResult(tuple):
    def __new__(cls, code, answer, transport_error=False):
        value = tuple.__new__(cls, (code, answer))
        value.transport_error = transport_error
        return value

def unavailable(answer):
    return ExchangeResult(1, answer, True)

def trace(event, detail=''):
    if os.environ.get('THEOURGIA_TRACE') not in (None, '', '0', 'off'):
        print(f'(trace {event} {detail})', file=sys.stderr, flush=True)

def encode_request(verb, argv):
    from local import scheme_string
    if not re.fullmatch(r'[a-zA-Z][a-zA-Z0-9_-]*', verb):
        verb = '|' + verb.replace('\\', '\\\\').replace('|', '\\|') + '|'
    return ('(' + verb + ''.join(' ' + scheme_string(arg) for arg in argv) + ')\n').encode()

def wrapped_request(store, actor, style, request):
    from local import scheme_string
    return ('(transport-v1 ' + ' '.join(map(scheme_string, [str(Path(store).resolve()), actor, style])) + ' ' + request.decode('utf-8').rstrip('\n') + ')\n').encode()

def unwrap(answer):
    match = re.fullmatch(rb'\(transport-answer ([01]) "([0-9a-f]*)"\)\n', answer)
    if not match:
        return unavailable(b'(error transport-invalid-answer)\n')
    try:
        return ExchangeResult(int(match[1]), bytes.fromhex(match[2].decode('ascii')))
    except ValueError:
        return unavailable(b'(error transport-invalid-answer)\n')

def exchange(store, actor, style, request, socket_path=None):
    from local import CORE, runtime_env
    packet = wrapped_request(store, actor, style, request)
    if len(packet) > FRAME_LIMIT:
        return unavailable(b'(error bad-request (reason frame-limit))\n')
    path = socket_path or str(Path(store) / 'socket')
    connected = False
    with socket.socket(socket.AF_UNIX) as peer:
        peer.settimeout(CONNECT_SECONDS)
        try:
            peer.connect(path)
            connected = True
        except OSError as exc:
            if exc.errno not in (errno.ENOENT, errno.ECONNREFUSED, errno.ENOTSOCK) and not isinstance(exc, TimeoutError):
                return unavailable(b'(error transport-unavailable)\n')
        if connected:
            trace('transport-forward', path)
            try:
                peer.settimeout(30)
                peer.sendall(packet)
                data = bytearray()
                while not data.endswith(b'\n'):
                    part = peer.recv(min(65536, ANSWER_LIMIT + 1 - len(data)))
                    if not part or len(data) + len(part) > ANSWER_LIMIT:
                        return unavailable(b'(error transport-unknown (reason incomplete-answer))\n')
                    data.extend(part)
                return unwrap(bytes(data))
            except OSError:
                # Once connected, sendall may have sent a prefix before failing.
                # Only an explicit same-identity caller retry may decide execution.
                return unavailable(b'(error transport-unknown (reason lost-answer))\n')
    trace('transport-local', path)
    worker = None
    try:
        worker = subprocess.Popen([runtime_env().get('THEOURGIA_SCHEME', 'scheme'), '--script', str(CORE/'rpc-worker.ss'), str(Path(store).resolve()), actor, '--once'],
                                  env=runtime_env(), stdin=subprocess.PIPE, stdout=subprocess.PIPE, start_new_session=True)
        output = bytearray()
        sent = 0
        deadline = time.monotonic() + 30
        with selectors.DefaultSelector() as ready:
            for port, event, name in [(worker.stdin, selectors.EVENT_WRITE, 'input'), (worker.stdout, selectors.EVENT_READ, 'output')]:
                os.set_blocking(port.fileno(), False)
                ready.register(port, event, name)
            while ready.get_map():
                if time.monotonic() >= deadline:
                    return unavailable(b'(error transport-unknown (reason local-deadline))\n')
                for key, _ in ready.select(.05):
                    if key.data == 'input':
                        try:
                            sent += os.write(key.fd, packet[sent:sent + 8192])
                        except BrokenPipeError:
                            sent = len(packet)
                        if sent == len(packet):
                            ready.unregister(key.fileobj)
                            worker.stdin.close()
                    else:
                        data = os.read(key.fd, 65536)
                        if not data:
                            ready.unregister(key.fileobj)
                        else:
                            if len(output) + len(data) > ANSWER_LIMIT:
                                return unavailable(b'(error transport-unknown (reason answer-limit))\n')
                            output.extend(data)
        worker.wait(timeout=max(.01, deadline-time.monotonic()))
        if worker.returncode:
            return unavailable(b'(error transport-unavailable)\n')
        return unwrap(bytes(output))
    except (OSError, subprocess.TimeoutExpired):
        return unavailable(b'(error transport-unknown (reason local-worker-exit))\n')
    finally:
        if worker:
            if worker.poll() is None:
                os.killpg(worker.pid, signal.SIGKILL)
            worker.wait()
            for port in (worker.stdin, worker.stdout):
                if port and not port.closed:
                    port.close()

class SocketOwnership:
    def __init__(self, path):
        self.path = Path(path)
        self.fd = None
        self.identity = None
    def claim(self):
        self.fd = os.open(str(self.path)+'.serve.lock', os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
        try:
            fcntl.flock(self.fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            raise ValueError('in-use')
        try:
            current = self.path.lstat()
        except FileNotFoundError:
            return
        if not stat.S_ISSOCK(current.st_mode) or current.st_uid != os.getuid():
            raise ValueError('occupied')
        with socket.socket(socket.AF_UNIX) as probe:
            probe.settimeout(CONNECT_SECONDS)
            try:
                probe.connect(str(self.path))
            except OSError as exc:
                if exc.errno != errno.ECONNREFUSED:
                    raise ValueError('unverifiable')
            else:
                raise ValueError('in-use')
        latest = self.path.lstat()
        if (latest.st_dev, latest.st_ino) != (current.st_dev, current.st_ino):
            raise ValueError('unverifiable')
        self.path.unlink()
    def bound(self):
        current = self.path.lstat()
        self.identity = current.st_dev, current.st_ino
        os.chmod(self.path, 0o600)
    def close(self):
        if self.identity:
            try:
                current = self.path.lstat()
                if (current.st_dev, current.st_ino) == self.identity:
                    self.path.unlink()
            except FileNotFoundError:
                pass
        if self.fd is not None:
            os.close(self.fd)
            self.fd = None

async def run_server(store, path, actor):
    from local import CORE, runtime_env
    ownership = SocketOwnership(path)
    server = worker = consumer = None
    clients = {}
    signals = 0
    draining = False
    queue = asyncio.Queue(maxsize=128)
    def count_signal():
        nonlocal signals
        signals += 1
    loop = asyncio.get_running_loop()
    for sig in (signal.SIGTERM, signal.SIGINT):
        loop.add_signal_handler(sig, count_signal)
    async def serve_queue():
        while True:
            frame, result = await queue.get()
            try:
                if draining:
                    answer = b'(error busy (reason draining))\n'
                else:
                    trace('daemon-dispatch', len(frame))
                    worker.stdin.write(frame)
                    await worker.stdin.drain()
                    answer = await worker.stdout.readline()
                    if not answer or not answer.endswith(b'\n') or len(answer) > ANSWER_LIMIT:
                        raise RuntimeError('worker-answer')
                if not result.done():
                    result.set_result(answer)
            except Exception:
                if not result.done():
                    result.set_result(b'(error transport-unknown (reason worker-exit))\n')
            finally:
                queue.task_done()
    async def peer(reader, writer):
        task = asyncio.current_task()
        clients[task] = 'reading'
        try:
            while not draining:
                clients[task] = 'reading'
                try:
                    frame = await asyncio.wait_for(reader.readline(), PEER_SECONDS)
                except (ValueError, asyncio.LimitOverrunError):
                    writer.write(b'(error bad-request (reason frame-limit))\n')
                    await writer.drain()
                    break
                except asyncio.TimeoutError:
                    break
                if not frame:
                    break
                if not frame.endswith(b'\n'):
                    answer = b'(error bad-request (reason incomplete-frame))\n'
                elif len(frame) > FRAME_LIMIT:
                    answer = b'(error bad-request (reason frame-limit))\n'
                else:
                    try:
                        frame.decode('utf-8', 'strict')
                    except UnicodeError:
                        answer = b'(error bad-request (reason invalid-utf8))\n'
                    else:
                        clients[task] = 'queued'
                        result = loop.create_future()
                        try:
                            queue.put_nowait((frame, result))
                            answer = await result
                        except asyncio.QueueFull:
                            answer = b'(error busy (reason queue-limit))\n'
                clients[task] = 'writing'
                writer.write(answer)
                await asyncio.wait_for(writer.drain(), PEER_SECONDS)
                if not frame.endswith(b'\n'):
                    break
        except (ConnectionError, asyncio.TimeoutError):
            pass
        finally:
            writer.close()
            clients.pop(task, None)
    try:
        ownership.claim()
        worker = await asyncio.create_subprocess_exec(runtime_env().get('THEOURGIA_SCHEME', 'scheme'), '--script', str(CORE/'rpc-worker.ss'), str(Path(store).resolve()), actor,
                                                       env=runtime_env(), stdin=asyncio.subprocess.PIPE, stdout=asyncio.subprocess.PIPE, limit=ANSWER_LIMIT,
                                                       start_new_session=True)
        consumer = asyncio.create_task(serve_queue())
        server = await asyncio.start_unix_server(peer, path=path, limit=FRAME_LIMIT)
        ownership.bound()
        trace('daemon-listening', path)
        while not signals and worker.returncode is None:
            await asyncio.sleep(.02)
        draining = True
        server.close()
        for task, phase in list(clients.items()):
            if phase == 'reading':
                task.cancel()
        deadline = loop.time() + DRAIN_SECONDS
        while (queue._unfinished_tasks or any(not t.done() for t in clients)) and signals < 2 and loop.time() < deadline:
            await asyncio.sleep(.02)
        if queue._unfinished_tasks or signals > 1 or loop.time() >= deadline:
            return 75
        return 0 if worker.returncode is None else 75
    except (ValueError, OSError) as exc:
        reason = str(exc) if isinstance(exc, ValueError) else 'unavailable'
        print(f'(error socket-unavailable (reason {reason}))')
        return 1
    finally:
        if server:
            server.close()
        pending_clients = list(clients)
        for task in pending_clients:
            task.cancel()
        if consumer:
            consumer.cancel()
        if worker:
            if worker.returncode is None:
                os.killpg(worker.pid, signal.SIGKILL)
            await worker.wait()
        await asyncio.gather(*pending_clients, *([consumer] if consumer else []), return_exceptions=True)
        if server:
            await server.wait_closed()
        ownership.close()
        for sig in (signal.SIGTERM, signal.SIGINT):
            loop.remove_signal_handler(sig)

def serve(args):
    import argparse
    parser = argparse.ArgumentParser(prog='theourgia serve')
    parser.add_argument('store')
    parser.add_argument('--socket')
    parser.add_argument('--actor', default=os.environ.get('THEOURGIA_ACTOR') or os.environ.get('USER') or 'cli')
    ns = parser.parse_args(args)
    return asyncio.run(run_server(ns.store, ns.socket or str(Path(ns.store)/'socket'), ns.actor))
