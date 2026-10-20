Console_eio.run ~plain:true prints on a terminal the lines a pipe gets, one
when a task starts, its output and one when it ends, rather than redrawing
rows in place, as a --progress=plain option asks. The example maps its
--plain to it.

  $ export CONSOLE_FROZEN_CLOCK=1
  $ ../tty.exe ../../../examples/progress/main.exe --plain
  [-] RUN make
    [-] sha256:abcd...
    # RUN make:   CC src/file0.c
    # RUN make:   CC src/file1.c
    # RUN make:   CC src/file2.c
    # RUN make:   CC src/file3.c
    # RUN make:   CC src/file4.c
    # RUN make:   CC src/file5.c
    [+] sha256:abcd...  2.00 GiB / 2.00 GiB  0.0s
  [+] RUN make  5/5  0.0s
  [+] Building 0.0s (2/2)

Without it, the same terminal gets the rows redrawn in place, and the screen
keeps the lines of the tasks that ended:

  $ ../tty.exe ../../../examples/progress/main.exe
    [+] sha256:abcd...                                 2.00 GiB / 2.00 GiB    0.0s
  [+] RUN make                                                         5/5    0.0s
  [+] Building 0.0s (2/2)
