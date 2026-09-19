"""Two real Scheme processes enter independent working spaces together."""
from pathlib import Path
import os
import selectors
import subprocess
import tempfile

import paths

core, lib = paths.core(), paths.libdir()
scratch = paths.scratch("working-processes-")
# THE EXTENSIONS ARE PINNED HERE TOO. Every other fixture sets them;
# this one inherited whatever Chez defaults to, which on the installed
# build does not include `.sc` -- so a library directory paths.py had
# accepted for holding `rpc.sc` would have resolved nothing here.
env = dict(os.environ, CHEZSCHEMELIBDIRS=str(lib),
           CHEZSCHEMELIBEXTS='.sc::.no-obj',
           THEOURGIA_HOME=str(scratch / "home"))
script = Path(__file__).resolve().parent / "working-process-child.sc"
store = str(scratch / "store")
setup = subprocess.run(["scheme", "--script", str(script), "setup", store], env=env,
                       capture_output=True, text=True, timeout=20, check=True,stdin=subprocess.DEVNULL)
block = setup.stdout.strip()
children = []
try:
    for writer, payload in [("agent001", "first"), ("agent002", "second")]:
        child = subprocess.Popen(["scheme", "--script", str(script), "write", store, writer, block, payload],
                                 env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE, text=True)
        children.append(child)
    with selectors.DefaultSelector() as ready:
        for child in children:
            ready.register(child.stdout, selectors.EVENT_READ)
        while ready.get_map():
            events = ready.select(20)
            if not events:
                raise TimeoutError("NO-READING: both children must reach the barrier")
            for key, _ in events:
                assert key.fileobj.readline() == "ready\n", "The child did not reach the product call"
                ready.unregister(key.fileobj)
    for child in children:
        child.stdin.write("x")
        child.stdin.flush()
    for child, payload in zip(children, ["first", "second"]):
        stdout, stderr = child.communicate(timeout=20)
        assert child.returncode == 0, stderr + stdout
        assert stdout == f'(ok (text "{payload}"))\n', stdout
    files = [Path(store) / "writers" / writer / "working" / block for writer in ["agent001", "agent002"]]
    assert files[0] != files[1]
    assert all(p.is_file() for p in files)
    print("ok WS-03 two actual processes preserve independent working bytes")
    print("working-processes complete")
finally:
    for child in children:
        if child.poll() is None:
            child.kill()
            child.wait()
