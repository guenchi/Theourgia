"""Bounded byte transport and Unix listener lifecycle; no verb semantics."""
from __future__ import annotations
import errno
import os
from pathlib import Path
import re
import selectors
import signal
import socket
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

# THE SERVER THAT USED TO BE HERE IS GONE, AND SO IS `SocketOwnership`.
#
# `theourgia serve` is Scheme now -- (theourgia daemon) -- and the
# command line reaches it itself. What stood here was a second daemon and
# a second implementation of the socket's ownership rules, in another
# language, kept in step with the first by hand.
#
# ⛔ WHAT IS LEFT IS `exchange`, AND IT HAS ONE CALLER: mcp/server.py.
# It is kept because that caller is still here, and it goes when that
# caller does. ⚠️ It speaks `(transport-v1 ...)` and expects
# `(transport-answer ...)`, which is NOT what the Scheme daemon speaks --
# measured: against a Scheme daemon it comes back
# `(error transport-invalid-answer)`. Its fall-back path, which runs a
# local worker when nothing is listening, still works.
