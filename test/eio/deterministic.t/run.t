CONSOLE_DETERMINISTIC_CLOCK, set to any value but the empty string, makes
the time of every Console_eio.run depend on the program alone, so a
transcript of a program's progress holds nothing of how fast the machine
ran it. The examples/progress program sleeps on the clock of its
environment, so its time stays at zero: every elapsed time reads 0.0s, no
rate shows, and the spinners and the motion of a scene stand still. The
example takes three seconds.

  $ export CONSOLE_DETERMINISTIC_CLOCK=1

On a pipe:

  $ ../../../examples/progress/main.exe 2>&1 | cat
  [-] RUN make
  [-] RUN make > sha256:abcd...
    # RUN make:   CC src/file0.c
    # RUN make:   CC src/file1.c
    # RUN make:   CC src/file2.c
    # RUN make:   CC src/file3.c
    # RUN make:   CC src/file4.c
    # RUN make:   CC src/file5.c
  [+] RUN make > sha256:abcd...  2.00 GiB / 2.00 GiB  0.0s
  [+] RUN make  5/5  0.0s
  [+] Building 0.0s (2/2)

On a terminal, the screen it leaves:

  $ ../tty.exe ../../../examples/progress/main.exe
  [+] RUN make                                                         5/5    0.0s
    +-- [+] sha256:abcd...                             2.00 GiB / 2.00 GiB    0.0s
  [+] Building 0.0s (2/2)

The examples/columns program sleeps on Console_eio.clock, whose time moves
by exactly each sleep: two of its runs print the same bytes, and the build
that sleeps 26 times 0.11 seconds takes 2.8s.

  $ ../../../examples/columns/main.exe > first 2>&1
  $ ../../../examples/columns/main.exe > second 2>&1
  $ cmp first second
  $ grep -e '] macos  ' -e Building first
  [+] macos  2.8s
  [+] Building 2.8s (9/9)

The empty string leaves the clock running:

  $ CONSOLE_DETERMINISTIC_CLOCK= ../../../examples/progress/main.exe 2>&1 | grep -c ' 0\.0s'
  0
  [1]
