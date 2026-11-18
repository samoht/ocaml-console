"""Run a program in a Windows pseudo console (ConPTY) and check what it drew.

Usage: python conpty.py [--line KEY=VALUE]... [--raw TEXT]... -- PROGRAM [ARGS...]

The pseudo console has 100 columns and 30 rows. The program's output is
printed as the console sent it and as the screen it leaves. Each --line
KEY=VALUE must appear as a word of the screen's first line, and each --raw
TEXT in the bytes the console sent back. The script exits with 1 if one is
missing.
"""

import argparse
import os
import sys

import pyte
from winpty import PtyProcess

COLS, ROWS = 100, 30

# The runner's console code page cannot encode every glyph the program draws.
sys.stdout.reconfigure(encoding="utf-8", errors="replace")

parser = argparse.ArgumentParser()
parser.add_argument("--line", action="append", default=[])
parser.add_argument("--raw", action="append", default=[])
parser.add_argument("command", nargs="+")
args = parser.parse_args()

proc = PtyProcess.spawn(args.command, dimensions=(ROWS, COLS), env=dict(os.environ))
chunks = []
while True:
    try:
        chunks.append(proc.read(65536))
    except EOFError:
        break
proc.wait()
raw = "".join(chunks)

screen = pyte.Screen(COLS, ROWS)
pyte.Stream(screen).feed(raw)
lines = [line.rstrip() for line in screen.display]
print(f"exit status: {proc.exitstatus}")
print("---- raw output ----")
print(repr(raw)[:4000])
print("---- screen ----")
print("\n".join(line for line in lines if line))

words = lines[0].split() if lines else []
missing = [f"--line {w}" for w in args.line if w not in words]
missing += [f"--raw {t!r}" for t in args.raw if t not in raw]
if proc.exitstatus != 0:
    missing.append(f"exit status {proc.exitstatus}")
for m in missing:
    print(f"MISSING: {m}")
sys.exit(1 if missing else 0)
