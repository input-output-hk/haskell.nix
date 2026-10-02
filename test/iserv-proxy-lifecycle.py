#!/usr/bin/env python3
"""Exercise the native proxy's process lifecycle without GHC or an emulator.

Run with --proxy /path/to/iserv-proxy. The same file acts as the fake
interpreter. Every fixture has a wall-clock bound and reaps its own processes.
"""

import argparse
import contextlib
import json
import os
from pathlib import Path
import select
import signal
import resource
import subprocess
import sys
import tempfile
import time


@contextlib.contextmanager
def ignore_missing_process():
    try:
        yield
    except ProcessLookupError:
        pass


def record_process(directory):
    with open(Path(directory) / "processes", "a", encoding="utf-8") as stream:
        stream.write(f"{os.getpid()} {os.getpgrp()}\n")
        stream.flush()


def fake_interpreter(scenario, directory, descriptors):
    record_process(directory)
    if scenario in ("exit_idle", "success_idle"):
        os._exit(139 if scenario == "exit_idle" else 0)

    if scenario == "fd_isolation":
        report = {}
        for descriptor in descriptors:
            try:
                status = os.fstat(descriptor)
                report[str(descriptor)] = [status.st_dev, status.st_ino]
            except OSError:
                report[str(descriptor)] = None
        (Path(directory) / "descriptors.json").write_text(json.dumps(report))

    if scenario in ("close_stdout_ignore_term", "ghc_disconnect", "interrupt", "terminate", "terminate_during_shutdown", "interrupt_during_shutdown"):
        signal.signal(signal.SIGTERM, signal.SIG_IGN)

    while True:
        request = os.read(0, 1)
        if request == b"\x0a":  # GHCi.Message.ResolveObjs; Binary Bool response.
            if scenario == "exit_after_request":
                os._exit(139)
            if scenario == "signal_after_request":
                resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
                os.kill(os.getpid(), signal.SIGSEGV)
            if scenario == "exit_with_descendant":
                child = os.fork()
                if child == 0:
                    signal.signal(signal.SIGTERM, signal.SIG_IGN)
                    record_process(directory)
                    (Path(directory) / "descendant-ready").touch()
                    time.sleep(60)
                    os._exit(0)
                while not (Path(directory) / "descendant-ready").exists():
                    time.sleep(0.01)
                os._exit(139)
            if scenario == "close_stdout_ignore_term":
                os.close(1)
                time.sleep(60)
                os._exit(0)
            os.write(1, b"\1")
        elif request == b"\0":  # Shutdown has no response bytes.
            if scenario in ("terminate_during_shutdown", "interrupt_during_shutdown"):
                (Path(directory) / "shutdown-started").touch()
                time.sleep(60)
            if scenario == "slow_shutdown":
                time.sleep(0.3)
            os._exit(23 if scenario == "shutdown_failure" else 0)
        elif not request:
            if scenario in ("ghc_disconnect", "interrupt", "terminate"):
                time.sleep(60)
            os._exit(0)
        else:
            raise AssertionError(f"Unexpected fake-interpreter request: {request!r}")


def cleanup_fixture(process, directory):
    # The patched proxy owns a separate interpreter process group. Explicitly
    # clean both groups if an assertion/timeout interrupts a broken proxy.
    groups = {process.pid} if process.poll() is None else set()
    processes = Path(directory) / "processes"
    if processes.exists():
        alive = set(live_owned_processes(directory))
        groups.update(
            int(line.split()[1]) for line in processes.read_text().splitlines()
            if int(line.split()[0]) in alive
        )
    for group in groups:
        if group != os.getpgrp():
            with ignore_missing_process():
                os.killpg(group, signal.SIGKILL)
    return process.communicate(timeout=3)


def live_owned_processes(directory):
    processes = Path(directory) / "processes"
    if not processes.exists():
        return []
    alive = []
    for line in processes.read_text().splitlines():
        pid = int(line.split()[0])
        try:
            # Linux can retain dead orphaned children briefly as zombies.
            status = Path(f"/proc/{pid}/status")
            if status.exists() and "\nState:\tZ" in status.read_text():
                continue
            os.kill(pid, 0)
            alive.append(pid)
        except (ProcessLookupError, FileNotFoundError):
            pass
    return alive


def run_case(proxy, scenario):
    with tempfile.TemporaryDirectory(prefix="iserv-proxy-lifecycle-") as directory:
        read_local, write_proxy = os.pipe()
        read_proxy, write_local = os.pipe()
        extra_read, extra_write = os.pipe()
        # A deliberately inheritable unrelated FD above normal interpreter FD
        # allocation proves that close_fds protects more than the two GHC pipes.
        extra_fd = 99
        os.dup2(extra_write, extra_fd, inheritable=True)
        os.close(extra_write)
        descriptors = (write_proxy, read_proxy, extra_fd)
        expected = {
            str(fd): [os.fstat(fd).st_dev, os.fstat(fd).st_ino]
            for fd in descriptors
        }
        command = [
            str(proxy), str(write_proxy), str(read_proxy), "--pipe",
            sys.executable, str(Path(__file__).resolve()), "--interpreter",
            scenario, directory, *(str(fd) for fd in descriptors), "-v",
        ]
        process = subprocess.Popen(
            command, pass_fds=descriptors, start_new_session=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        )
        os.close(write_proxy)
        os.close(read_proxy)
        os.close(extra_fd)
        os.close(extra_read)
        start = time.monotonic()
        response = None
        try:
            if scenario not in ("exit_idle", "success_idle"):
                os.write(write_local, b"\x0a")
                if scenario not in ("exit_after_request", "signal_after_request", "exit_with_descendant", "close_stdout_ignore_term"):
                    ready, _, _ = select.select([read_local], [], [], 5)
                    assert ready, f"{scenario}: no ResolveObjs response within 5 seconds"
                    response = os.read(read_local, 1)
                    assert response == b"\1", f"{scenario}: wrong response {response!r}"
                    if scenario == "ghc_disconnect":
                        os.close(write_local)
                        write_local = None
                    elif scenario == "interrupt":
                        os.kill(process.pid, signal.SIGINT)
                    elif scenario == "terminate":
                        os.kill(process.pid, signal.SIGTERM)
                    else:
                        os.write(write_local, b"\0")
                        if scenario in ("terminate_during_shutdown", "interrupt_during_shutdown"):
                            deadline = time.monotonic() + 5
                            while not (Path(directory) / "shutdown-started").exists():
                                assert time.monotonic() < deadline, "Shutdown did not reach interpreter"
                                time.sleep(0.01)
                            cancel_signal = signal.SIGTERM if scenario == "terminate_during_shutdown" else signal.SIGINT
                            os.kill(process.pid, cancel_signal)
            stdout, stderr = process.communicate(timeout=12)
            elapsed = time.monotonic() - start
            diagnostic = stderr.decode(errors="replace")
            deadline = time.monotonic() + 1
            survivors = live_owned_processes(directory)
            while survivors and time.monotonic() < deadline:
                time.sleep(0.01)
                survivors = live_owned_processes(directory)
            assert not survivors, f"{scenario}: interpreter processes survived cleanup: {survivors}"
            successful = scenario in ("normal", "slow_shutdown", "fd_isolation")
            assert (process.returncode == 0) == successful, (
                f"{scenario}: exit {process.returncode}, stderr={diagnostic!r}"
            )
            if scenario in ("exit_idle", "exit_after_request", "exit_with_descendant"):
                assert "139" in diagnostic, f"{scenario}: child exit status lost: {diagnostic!r}"
            if scenario == "signal_after_request":
                assert "-11" in diagnostic, f"{scenario}: child signal lost: {diagnostic!r}"
            if scenario == "success_idle":
                assert "ExitSuccess" in diagnostic, f"{scenario}: unexpected success not reported: {diagnostic!r}"
            if scenario == "shutdown_failure":
                assert "23" in diagnostic, f"{scenario}: shutdown failure lost: {diagnostic!r}"
            if scenario == "slow_shutdown":
                assert elapsed >= 0.3, "Graceful shutdown was terminated before completion"
            if scenario == "fd_isolation":
                observed = json.loads((Path(directory) / "descriptors.json").read_text())
                assert all(observed[str(fd)] != expected[str(fd)] for fd in descriptors), (
                    f"Interpreter inherited GHC/unrelated pipe descriptors: {observed!r}"
                )
            print(f"PASS {scenario}: exit={process.returncode}, {elapsed:.3f}s", flush=True)
        except (AssertionError, subprocess.TimeoutExpired) as error:
            stdout, stderr = cleanup_fixture(process, directory)
            raise AssertionError(
                f"{scenario}: {error}; stdout={stdout.decode(errors='replace')!r}, "
                f"stderr={stderr.decode(errors='replace')!r}"
            ) from error
        finally:
            cleanup_fixture(process, directory)
            os.close(read_local)
            if write_local is not None:
                os.close(write_local)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--proxy", type=Path)
    parser.add_argument("--case", action="append", dest="cases")
    parser.add_argument("--repeat", type=int, default=1)
    parser.add_argument("--interpreter", nargs="+")
    parser.add_argument("-v", action="store_true")
    args = parser.parse_args()
    if args.interpreter:
        scenario, directory, *descriptors = args.interpreter
        fake_interpreter(scenario, directory, [int(fd) for fd in descriptors if fd != "-v"])
    else:
        if args.proxy is None:
            parser.error("--proxy is required")
        if os.name != "posix":
            parser.error("The lifecycle fixtures require POSIX processes and file descriptors")
        if args.repeat < 1:
            parser.error("--repeat must be positive")
        cases = args.cases or [
            "normal", "slow_shutdown", "exit_after_request", "signal_after_request", "exit_idle",
            "success_idle", "exit_with_descendant", "close_stdout_ignore_term",
            "ghc_disconnect", "interrupt", "terminate", "terminate_during_shutdown", "interrupt_during_shutdown",
            "shutdown_failure", "fd_isolation",
        ]
        for _ in range(args.repeat):
            for scenario in cases:
                run_case(args.proxy.resolve(), scenario)


if __name__ == "__main__":
    main()
