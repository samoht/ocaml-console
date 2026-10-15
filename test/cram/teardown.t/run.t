A live display hides the terminal's cursor, and the last cursor sequence it
leaves on the terminal shows it again, however the command ends: [h] below is
the last of [ESC [ ? 25 h] (shown) and [ESC [ ? 25 l] (hidden) in the output.

  $ last_cursor () { cat -v out | tr -d '\n' | sed 's/.*\^\[\[?25\([hl]\).*/\1/'; echo; }

A display that finishes:

  $ teardown.exe ok > out; echo "exit $?"
  exit 0
  $ last_cursor
  h

A command that calls [exit] inside the display, as a CLI's error path does:

  $ teardown.exe exit > out; echo "exit $?"
  exit 3
  $ last_cursor
  h

An exception that escapes the command:

  $ teardown.exe raise > out 2> err; echo "exit $?"
  exit 2
  $ last_cursor
  h
