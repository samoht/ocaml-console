A --progress=plain option asks Console_eio.run to print on a terminal the
lines a pipe gets, one when a task starts, its output and one when it ends,
rather than redrawing rows in place. The example reads it with
Console_eio.progress, as Cmdliner.Arg.enum does.

  $ export CONSOLE_FROZEN_CLOCK=1
  $ ../tty.exe ../../../examples/progress/main.exe --progress=plain
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

With --progress=auto, the default, the same terminal gets the rows redrawn
in place, and the screen keeps the lines of the tasks that ended:

  $ ../tty.exe ../../../examples/progress/main.exe --progress=auto
    [+] sha256:abcd...                                 2.00 GiB / 2.00 GiB    0.0s
  [+] RUN make                                                         5/5    0.0s
  [+] Building 0.0s (2/2)

  $ ../tty.exe ../../../examples/progress/main.exe
    [+] sha256:abcd...                                 2.00 GiB / 2.00 GiB    0.0s
  [+] RUN make                                                         5/5    0.0s
  [+] Building 0.0s (2/2)
