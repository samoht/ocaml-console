"""Run a program in a Windows pseudo console (ConPTY) and show what it drew.

Usage: python conpty.py PROGRAM [ARGS...]

It prints the bytes the console sent back, then the screen they leave, as a
terminal of 100 columns and 30 rows shows it.
"""

import os
import sys

import pyte
from winpty import PtyProcess

COLS, ROWS = 100, 30

proc = PtyProcess.spawn(sys.argv[1:], dimensions=(ROWS, COLS), env=dict(os.environ))
chunks = []
while True:
    try:
        chunks.append(proc.read(65536))
    except EOFError:
        break
proc.wait()
raw = "".join(chunks)

print(f"exit status: {proc.exitstatus}")
print("---- raw output ----")
print(repr(raw)[:6000])
screen = pyte.Screen(COLS, ROWS)
pyte.Stream(screen).feed(raw)
print("---- final screen ----")
for line in screen.display:
    print(line.rstrip())
