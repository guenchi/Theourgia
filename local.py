#!/usr/bin/env python3
"""Local process lifecycle. Business verbs remain in the Scheme dispatcher."""
from __future__ import annotations
import argparse
import os
from pathlib import Path
import selectors
import signal
import subprocess
import sys
import time

CORE = Path(__file__).resolve().parent

def scheme_string(text: str) -> str:
    return '"' + ''.join({'"': '\\"', '\\': '\\\\', '\n': '\\n', '\r': '\\r', '\t': '\\t'}.get(c, c if ord(c) >= 32 else '\\x%x;' % ord(c)) for c in text) + '"'

def runtime_env():
    env = dict(os.environ)
    env['CHEZSCHEMELIBDIRS'] = os.pathsep.join([str(CORE.parent), env.get('CHEZSCHEMELIBDIRS', '')])
    env['CHEZSCHEMELIBEXTS'] = '.ss::.no-obj:.sc::.no-obj'
    return env

def terminate(worker):
    if worker.poll() is None:
        os.killpg(worker.pid, signal.SIGKILL)
    worker.wait()

def rss_bytes(pid):
    result = subprocess.run(['ps', '-o', 'rss=', '-p', str(pid)], capture_output=True, timeout=0.5)
    return int(result.stdout.strip() or b'0') * 1024

def evaluate(args):
    parser = argparse.ArgumentParser(prog='theourgia eval', exit_on_error=False)
    parser.add_argument('--store', default=os.environ.get('THEOURGIA_STORE', '.'))
    parser.add_argument('--cut', default='')
    parser.add_argument('--under', default='')
    parser.add_argument('--timeout-ms', type=int, default=3000)
    parser.add_argument('--memory-bytes', type=int, default=268435456)
    parser.add_argument('--output-bytes', type=int, default=65536)
    parser.add_argument('source', nargs='?')
    try:
        ns = parser.parse_args(args)
        if not (1 <= ns.timeout_ms <= 60000 and 1048576 <= ns.memory_bytes <= 2147483648 and 128 <= ns.output_bytes <= 1048576):
            raise ValueError('limits')
        source = ns.source.encode() if ns.source is not None else sys.stdin.buffer.read(1048577)
        if len(source) > 1048576:
            return b'(error bad-source (reason input-limit))\n'
    except (ValueError, argparse.ArgumentError, SystemExit):
        return b'(error bad-request (reason eval-arguments))\n'
    read_fd, write_fd = os.pipe()
    user_error_read, user_error_write = os.pipe()
    env = runtime_env()
    env.update(THEOURGIA_EVAL_FD=str(write_fd), THEOURGIA_EVAL_OUTPUT=str(ns.output_bytes), THEOURGIA_EVAL_ERROR_FD=str(user_error_write))
    deadline = time.monotonic() + ns.timeout_ms / 1000
    worker = None
    try:
        worker = subprocess.Popen([env.get('THEOURGIA_SCHEME', 'scheme'), '--script', str(CORE / 'eval-worker.ss'), ns.store, ns.cut, ns.under],
                                  env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                  pass_fds=(write_fd, user_error_write), start_new_session=True)
        os.close(write_fd)
        os.close(user_error_write)
        user_error_write = -1
        write_fd = -1
        # Input is nonblocking as a child can exit before reading it.
        streams = {'protocol': bytearray(), 'stdout': bytearray(), 'stderr': bytearray(), 'diagnostics': bytearray()}
        resource = None
        total = 0
        next_memory = time.monotonic()
        with selectors.DefaultSelector() as ready:
            for stream, name in [(read_fd, 'protocol'), (worker.stdout, 'stdout'), (worker.stderr, 'diagnostics'), (user_error_read, 'stderr')]:
                os.set_blocking(stream if isinstance(stream, int) else stream.fileno(), False)
                ready.register(stream, selectors.EVENT_READ, name)
            os.set_blocking(worker.stdin.fileno(), False)
            ready.register(worker.stdin, selectors.EVENT_WRITE, 'input')
            sent = 0
            while ready.get_map():
                now = time.monotonic()
                if now >= deadline:
                    resource = 'time'
                    break
                if now >= next_memory:
                    if rss_bytes(worker.pid) > ns.memory_bytes:
                        resource = 'memory'
                        break
                    next_memory = now + 0.05
                for key, _ in ready.select(min(0.025, max(0, deadline - now))):
                    fd = key.fd
                    if key.data == 'input':
                        try:
                            sent += os.write(fd, source[sent:sent + 8192])
                        except BrokenPipeError:
                            sent = len(source)
                        if sent == len(source):
                            ready.unregister(key.fileobj)
                            worker.stdin.close()
                        continue
                    chunk = os.read(fd, min(8192, ns.output_bytes + 1))
                    if not chunk:
                        ready.unregister(key.fileobj)
                        continue
                    total += len(chunk)
                    if total > ns.output_bytes:
                        resource = 'output'
                        break
                    streams[key.data].extend(chunk)
                if resource:
                    break
        if resource:
            terminate(worker)
            limit = {'time': ns.timeout_ms, 'memory': ns.memory_bytes, 'output': ns.output_bytes}[resource]
            return (f'(error eval-limit (resource {resource}) (limit {limit}) (stdout ' + scheme_string(streams['stdout'].decode('utf-8', 'replace')) + '))\n').encode()
        worker.wait(timeout=max(0.01, deadline - time.monotonic()))
        sys.stderr.buffer.write(streams['diagnostics'])
        protocol = bytes(streams['protocol'])
        if worker.returncode or not protocol.endswith(b'\n'):
            return f'(error eval-worker-exit (status {worker.returncode}))\n'.encode()
        if protocol.startswith(b'(ok ') and protocol.endswith(b')\n'):
            return protocol[:-2] + (' (stdout ' + scheme_string(streams['stdout'].decode('utf-8', 'replace')) + ') (stderr ' + scheme_string(streams['stderr'].decode('utf-8', 'replace')) + '))\n').encode()
        return protocol
    except subprocess.TimeoutExpired:
        return f'(error eval-limit (resource time) (limit {ns.timeout_ms}))\n'.encode()
    except (OSError, ValueError):
        return b'(error eval-worker-unavailable)\n'
    finally:
        if worker is not None:
            terminate(worker)
            for stream in (worker.stdin, worker.stdout, worker.stderr):
                if stream and not stream.closed:
                    stream.close()
        os.close(read_fd)
        os.close(user_error_read)
        if user_error_write >= 0:
            os.close(user_error_write)
        if write_fd >= 0:
            os.close(write_fd)

def main(argv):
    if argv and argv[0] == 'serve':
        from transport import serve
        return serve(argv[1:])
    if argv and argv[0] == '--forward':
        from transport import exchange
        store, actor, style, socket_path, request = argv[1:]
        code, answer = exchange(store, actor, style, request.encode(), socket_path)
        sys.stdout.buffer.write(answer)
        return code
    if argv and argv[0] == 'eval':
        answer = evaluate(argv[1:])
        sys.stdout.buffer.write(answer)
        return 0 if answer.startswith(b'(ok ') else 1
    return subprocess.call([runtime_env().get('THEOURGIA_SCHEME', 'scheme'), '--script', str(CORE / 'cli.ss'), *argv], env=runtime_env())

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
