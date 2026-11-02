The size of the terminal a formatter writes to, for a display of one's own:

  $ ../piped.exe size 2>/dev/null
  no terminal

  $ ../tty.exe ../piped.exe size
  80x24

On a pipe, what the program prints above the rows is laid out as wide as
$COLUMNS says, as Python's shutil.get_terminal_size, Rich and clap read
it, so that a table piped to another program fits the width it was asked
for:

  $ COLUMNS=30 ../piped.exe print 2>&1 >/dev/null
  ------------------------------
  +----------------------------+
  | package                    |
  +----------------------------+
  | a description long enough  |
  | to wrap in a narrow table  |
  +----------------------------+

Without $COLUMNS it is printed whole:

  $ COLUMNS= ../piped.exe print 2>&1 >/dev/null
  ----
  +-----------------------------------------------------+
  | package                                             |
  +-----------------------------------------------------+
  | a description long enough to wrap in a narrow table |
  +-----------------------------------------------------+

A program that lays out its own lines reads the same width: the
terminal's, else $COLUMNS, else none, where it prints whole:

  $ ../tty.exe ../piped.exe width
  80

  $ COLUMNS=30 ../piped.exe width
  30

  $ COLUMNS= ../piped.exe width
  whole

  $ COLUMNS=5000 ../piped.exe width
  whole
