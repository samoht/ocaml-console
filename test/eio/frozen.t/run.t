CONSOLE_FROZEN_CLOCK, set to any value but the empty string, stops the clock
of every Console_eio.run at zero, so a transcript of a program's progress
holds the program's output and nothing of how fast the machine ran it: every
elapsed time reads 0.0s, and the rates, the spinners and the motion of a
scene stand still. The example takes three seconds.

  $ export CONSOLE_FROZEN_CLOCK=1

On a pipe:

  $ ../../../examples/progress/main.exe 2>&1 | cat
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

On a terminal, the screen it leaves:

  $ ../tty.exe ../../../examples/progress/main.exe
    [+] sha256:abcd...                                 2.00 GiB / 2.00 GiB    0.0s
  [+] RUN make                                                         5/5    0.0s
  [+] Building 0.0s (2/2)

The empty string leaves the clock running:

  $ CONSOLE_FROZEN_CLOCK= ../../../examples/progress/main.exe 2>&1 | grep -c ' 0\.0s'
  0
  [1]
