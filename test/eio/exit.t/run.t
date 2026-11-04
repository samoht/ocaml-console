A program that calls exit inside Console_eio.run has the display closed by
an at_exit function, its task cancelled. What an at_exit function that runs
after it logs goes to the reporter installed before the run, and what it
prints goes out as without console, on a pipe:

  $ ../piped.exe exit 2>&1
  [-] work
  result 1
  [~] work  0.0s  cancelled
  piped.exe: [WARNING] logged at exit
  printed at exit
  result at exit
  [3]

and on a terminal:

  $ ../tty.exe ../piped.exe exit
  [~] work   0.0s
    # result 1
  piped.exe: [WARNING] logged at exit
  printed at exit
  result at exit

A signal ends the program as it asks, also once a view raised:

  $ ../piped.exe signal 2>/dev/null
  [130]

So does one that comes while the scheduler waits, with the display on an Eio
flow, which a signal handler cannot write to, and the cursor is shown:

  $ ../piped.exe idle-signal idle.out 2>/dev/null
  [130]
  $ grep -c '25h' idle.out
  1

A view that calls exit leaves the cursor shown:

  $ ../piped.exe exit-view view.out 2>/dev/null
  [3]
  $ grep -c '25h' view.out
  1
