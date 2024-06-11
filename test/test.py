#!/bin/env python3

import argparse
import asyncio
import multiprocessing
import os
import shutil
import tempfile
from asyncio.subprocess import Process
from pathlib import Path


# https://stackoverflow.com/questions/287871/how-do-i-print-colored-text-to-the-terminal
class bcolors:
    HEADER = "\033[95m"
    OKBLUE = "\033[94m"
    OKCYAN = "\033[96m"
    OKGREEN = "\033[92m"
    WARNING = "\033[93m"
    FAIL = "\033[91m"
    ENDC = "\033[0m"
    BOLD = "\033[1m"
    UNDERLINE = "\033[4m"


output_func_queue = asyncio.Queue()
fails = []


async def run_queue_in_seq():
    while True:
        item = await output_func_queue.get()
        if item is None:
            return
        await item()


async def run_one_test(file: Path, outpath: Path, process: Process):

    async def show_stdout():
        stdout = process.stdout
        assert stdout is not None
        aggr = []
        async for line in stdout:
            aggr.append(line)
            print(f"{file.name}>> {line.decode(errors='replace')}", end="")
        return b"".join(aggr)

    async def show_stderr():
        stderr = process.stderr
        assert stderr is not None
        aggr = []
        async for line in stderr:
            aggr.append(line)
            print(f"{file.name}> {line.decode(errors='replace')}", end="")
        return b"".join(aggr)

    msgs_list = []

    async def wait_one():
        code, out, err = await asyncio.gather(
            process.wait(), show_stdout(), show_stderr()
        )
        print()
        for x in msgs_list:
            print(x)
        if not code == 0:
            fails.append(str(file))
            print(f"{bcolors.FAIL}{file.name}: Failed with code {code}{bcolors.ENDC}")
            if out:
                outpath.with_suffix(".stdout").write_bytes(out)
            if err:
                outpath.with_suffix(".stderr").write_bytes(err)
            print()

    await output_func_queue.put(wait_one)
    try:
        await asyncio.wait_for(process.wait(), timeout=30)
    except asyncio.TimeoutError:
        msgs_list.append(
            f"{bcolors.FAIL}{file.name}: terminating due to timeout{bcolors.ENDC}"
        )
        process.terminate()
        await asyncio.sleep(1)
        if process.returncode is None:
            msgs_list.append(f"{bcolors.FAIL}{file.name}: killing{bcolors.ENDC}")
            process.kill()


async def main():
    parser = argparse.ArgumentParser()

    parser.add_argument("files", nargs="*")
    parser.add_argument("-l", "--lilypond", type=str, default=shutil.which("lilypond"))
    parser.add_argument("-o", "--out", type=str, required=True)
    parser.add_argument("-i", "--include", type=str, action="append", default=[])
    parser.add_argument("--cpu", type=int, default=multiprocessing.cpu_count())

    args = parser.parse_args()

    lily_bin = Path(args.lilypond).absolute()
    assert lily_bin.exists()
    print(f"using {lily_bin}")

    if args.files:
        args_files = [Path(x).absolute() for x in sorted(args.files)]
    else:
        args_files = [Path(__file__).parent / "tests"]

    seen = set()

    def walk(p: Path):
        p = p.resolve()
        if p not in seen:
            seen.add(p)
            if p.is_dir():
                for c in p.iterdir():
                    yield from walk(c)
            elif p.suffix == ".ly":
                yield p

    files: list[tuple[Path, Path]] = []
    out_subdirs = set()
    for file in args_files:
        for x in walk(file):
            rel = x.relative_to(file)
            assert not ".." in rel.parts
            if rel in out_subdirs:
                raise RuntimeError(f"conflicting output: {rel}")
            out_subdirs.add(rel)
            files.append((x, rel))

    files = sorted(files)

    print(f"running {len(files)} tests")

    ly_args = [
        "-I",
        str(Path(__file__).parent),
    ]
    includes = args.include

    if len(includes) > 1:
        raise NotImplementedError()

    for x in includes:
        ly_args.append("-d")
        ly_args.append(f"include-settings={x}")

    sem = asyncio.Semaphore(args.cpu)

    outpath = Path(args.out).resolve()
    print(f"outputing to {outpath}")
    if not outpath.exists():
        outpath.mkdir(parents=True)

    assert outpath.is_dir()

    with tempfile.TemporaryDirectory() as tmp_dir:

        print(f"using tmp home: {tmp_dir}")

        async def test_one(file: Path, outrel: Path):
            async with sem:
                outrel = outpath / outrel.with_suffix("")
                outrel.parent.mkdir(parents=True, exist_ok=True)
                proc = await asyncio.create_subprocess_exec(
                    lily_bin,
                    "--output",
                    str(outrel),
                    *ly_args,
                    str(file),
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                    cwd="/",
                    env={
                        # for fontconfig and guile cache
                        "HOME": tmp_dir,
                        "CLAIRNOTE": os.environ.get("CLAIRNOTE") or "default",
                    },
                )
                await run_one_test(file, outrel, proc)

        jobs = []
        for file, outrel in files:
            jobs.append(test_one(file, outrel))

        async def run_jobs():
            await asyncio.gather(*jobs)
            await output_func_queue.put(None)

        try:
            await asyncio.gather(run_queue_in_seq(), run_jobs())
        except KeyboardInterrupt:
            print("[KeyboardInterrupt]")
        finally:
            if fails:
                print()
                print(f"{len(fails)} failed:")
                for f in fails:
                    print(f)


loop = asyncio.new_event_loop()
asyncio.set_event_loop(loop)
try:
    loop.run_until_complete(main())
except KeyboardInterrupt:
    tasks = [t for t in asyncio.all_tasks(loop) if not t.done()]
    [task.cancel() for task in tasks]
    loop.run_until_complete(asyncio.gather(*tasks, return_exceptions=True))
finally:
    loop.close()
