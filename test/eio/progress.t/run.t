A --progress option asks Console_eio.run how progress shows, as docker
build's does. The example reads it with Console.Progress.all, as
Cmdliner.Arg.enum does.

  $ export CONSOLE_DETERMINISTIC_CLOCK=1

With --progress=auto, the default, a terminal gets the rows redrawn in
place, and the screen keeps the tree of the tasks that ended, then the
summary:

  $ ../tty.exe ../../../examples/progress/main.exe --progress=auto
  [+] RUN make                                                         5/5    0.0s
    +-- [+] sha256:abcd...                             2.00 GiB / 2.00 GiB    0.0s
  [+] Building 0.0s (2/2)

  $ ../tty.exe ../../../examples/progress/main.exe
  [+] RUN make                                                         5/5    0.0s
    +-- [+] sha256:abcd...                             2.00 GiB / 2.00 GiB    0.0s
  [+] Building 0.0s (2/2)

A pipe gets plain lines instead, one when a task starts, its output and one
when it ends, then the summary:

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

--progress=plain prints those lines on a terminal too:

  $ ../tty.exe ../../../examples/progress/main.exe --progress=plain
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

--progress=tty redraws the rows on a pipe too, such as one a terminal
recorder reads: the bytes leave the screen a terminal shows. The rows are
80 columns wide unless COLUMNS says otherwise:

  $ ../../../examples/progress/main.exe --progress=tty 2>&1 | ../tty.exe -
  [+] RUN make                                                         5/5    0.0s
    +-- [+] sha256:abcd...                             2.00 GiB / 2.00 GiB    0.0s
  [+] Building 0.0s (2/2)

  $ COLUMNS=50 ../../../examples/progress/main.exe --progress=tty 2>&1 \
  >   | ../tty.exe -
  [+] RUN make                           5/5    0.0s
    +-- [+] sha256:abcd...           2.0G/2.0G  0.0s
  [+] Building 0.0s (2/2)

--progress=quiet prints nothing when the work succeeds, and a failed task's
line and its output when it fails. With --fail the layer fails, and the
example fails its step too, since a task under it does not:

  $ ../tty.exe ../../../examples/progress/main.exe --progress=quiet

  $ ../tty.exe ../../../examples/progress/main.exe --progress=quiet --fail
  [x] RUN make > sha256:abcd...  2.00 GiB / 2.00 GiB  0.0s  failed
    # checksum mismatch
  [x] RUN make  5/5  0.0s  failed
    #   CC src/file0.c
    #   CC src/file1.c
    #   CC src/file2.c
    #   CC src/file3.c
    #   CC src/file4.c
    #   CC src/file5.c

The example then exits with 1:

  $ ../../../examples/progress/main.exe --progress=quiet --fail
  [x] RUN make > sha256:abcd...  2.00 GiB / 2.00 GiB  0.0s  failed
    # checksum mismatch
  [x] RUN make  5/5  0.0s  failed
    #   CC src/file0.c
    #   CC src/file1.c
    #   CC src/file2.c
    #   CC src/file3.c
    #   CC src/file4.c
    #   CC src/file5.c
  [1]

--progress=json writes one JSON object on each line for each event, on a
terminal as on a pipe:

  $ ../../../examples/progress/main.exe --progress=json --fail 2>&1 | cat
  {"time":0,"event":"start","id":0,"task":["RUN make"]}
  {"time":0,"event":"start","id":1,"task":["RUN make","sha256:abcd..."],"parent":0}
  {"time":0,"event":"progress","id":0,"task":["RUN make"],"count":0,"total":5,"unit":"items"}
  {"time":0,"event":"output","id":0,"task":["RUN make"],"text":"  CC src/file0.c"}
  {"time":0,"event":"progress","id":0,"task":["RUN make"],"count":1,"total":5,"unit":"items"}
  {"time":0,"event":"output","id":0,"task":["RUN make"],"text":"  CC src/file1.c"}
  {"time":0,"event":"progress","id":0,"task":["RUN make"],"count":2,"total":5,"unit":"items"}
  {"time":0,"event":"output","id":0,"task":["RUN make"],"text":"  CC src/file2.c"}
  {"time":0,"event":"progress","id":0,"task":["RUN make"],"count":3,"total":5,"unit":"items"}
  {"time":0,"event":"output","id":0,"task":["RUN make"],"text":"  CC src/file3.c"}
  {"time":0,"event":"progress","id":0,"task":["RUN make"],"count":4,"total":5,"unit":"items"}
  {"time":0,"event":"output","id":0,"task":["RUN make"],"text":"  CC src/file4.c"}
  {"time":0,"event":"progress","id":0,"task":["RUN make"],"count":5,"total":5,"unit":"items"}
  {"time":0,"event":"output","id":0,"task":["RUN make"],"text":"  CC src/file5.c"}
  {"time":0,"event":"progress","id":1,"task":["RUN make","sha256:abcd..."],"count":0,"total":2147483648,"unit":"bytes"}
  {"time":0,"event":"progress","id":1,"task":["RUN make","sha256:abcd..."],"count":268435456,"total":2147483648,"unit":"bytes"}
  {"time":0,"event":"progress","id":1,"task":["RUN make","sha256:abcd..."],"count":536870912,"total":2147483648,"unit":"bytes"}
  {"time":0,"event":"progress","id":1,"task":["RUN make","sha256:abcd..."],"count":805306368,"total":2147483648,"unit":"bytes"}
  {"time":0,"event":"progress","id":1,"task":["RUN make","sha256:abcd..."],"count":1073741824,"total":2147483648,"unit":"bytes"}
  {"time":0,"event":"progress","id":1,"task":["RUN make","sha256:abcd..."],"count":1342177280,"total":2147483648,"unit":"bytes"}
  {"time":0,"event":"progress","id":1,"task":["RUN make","sha256:abcd..."],"count":1610612736,"total":2147483648,"unit":"bytes"}
  {"time":0,"event":"progress","id":1,"task":["RUN make","sha256:abcd..."],"count":1879048192,"total":2147483648,"unit":"bytes"}
  {"time":0,"event":"progress","id":1,"task":["RUN make","sha256:abcd..."],"count":2147483648,"total":2147483648,"unit":"bytes"}
  {"time":0,"event":"output","id":1,"task":["RUN make","sha256:abcd..."],"text":"checksum mismatch"}
  {"time":0,"event":"end","id":1,"task":["RUN make","sha256:abcd..."],"status":"failed","elapsed":0}
  {"time":0,"event":"end","id":0,"task":["RUN make"],"status":"failed","elapsed":0}

Console.Progress.Event.jsont reads each of these events back, and writes it
as it was:

  $ ../../../examples/progress/main.exe --progress=json --fail 2>&1 \
  >   | ../events.exe
  26 events read back
