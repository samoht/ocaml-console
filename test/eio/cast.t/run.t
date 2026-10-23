Under CONSOLE_DETERMINISTIC_CLOCK, CONSOLE_CAST=FILE records what a run
writes on the terminal to FILE, in asciicast v2, rather than on the
output: a header, then one [time, "o", data] line per write, at its time
on the virtual clock. The terminal is $COLUMNS by $LINES, with no pty
behind it, and the program's sleeps take no real time.

  $ export CONSOLE_DETERMINISTIC_CLOCK=1 TERM=xterm-256color LANG=en_US.UTF-8
  $ export COLUMNS=80 LINES=24
  $ CONSOLE_CAST=columns.cast ../../../examples/columns/main.exe
  $ head -1 columns.cast
  {"version":2,"width":80,"height":24,"env":{"TERM":"xterm-256color"}}

The two builds of examples/columns take the time of the longer, 26 sleeps
of 0.11 seconds, when the last write comes:

  $ tail -1 columns.cast | cut -c 1-12
  [2.86,"o","\

Two recordings are the same bytes:

  $ CONSOLE_CAST=again.cast ../../../examples/columns/main.exe
  $ cmp columns.cast again.cast

The recording leaves the screen that the program leaves on a terminal:

  $ ../tty.exe ../../../examples/columns/main.exe > terminal
  $ ../tty.exe --cast columns.cast > recorded
  $ cmp terminal recorded
  $ grep Building recorded
  [+] Building 2.9s (9/9)
