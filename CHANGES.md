## 0.9.0

Initial release of `console` and `console-eio`.

- Styled text. `Style` combines bold, italic, underline and colours with
  `+`; `Color` names the 16 ANSI colours, the xterm 256-colour palette and
  RGB, written at the colour depth of their formatter (`Color.set_depth`, 16
  colours until one is set). A `Span.t` is text in one or more styles, with
  links (`Span.link`, percent-encoding the bytes a URI may not hold) and
  colours graded along it (`Span.gradient`). A span holds only text that
  prints, and `Style.styled` styles any `Fmt` printer.
- Widgets. `Table` (headers and cells are spans, columns that wrap or
  truncate, rules between rows with `~rules:true`, a frameless
  `Border.simple`), `Tree`, `Panel`, `Rule` and `Bar`. Every widget is a
  span too, by its `to_span`, laid out when it is printed in the theme of
  the formatter that prints it, so widgets nest: a bar or a table in a
  cell, a table in a panel, and `Span.hcat` puts spans, widgets among
  them, side by side, as one more span, and `Span.vcat` one under the
  other (`Span.height` counts its lines). Widths are counted in terminal
  cells, by grapheme cluster with uuseg and uucp, wide characters and emoji
  included, a widget is no wider than its formatter's margin, and every
  widget is drawn in ASCII on a formatter that is not UTF-8. Each has a
  `pp`, a `to_span` and a `to_string ?styled`.
- Themes. A theme (`Theme.set`, kept on the formatter as its colours are)
  gives every widget its colours, marks, frames, tree lines, bars and
  spinner. Eighteen are predefined: `Theme.default`, which reads on any
  background, `ascii`, `dos`, `matrix`, `rainbow`, and the colour schemes of
  Dracula, Nord, Solarized, gruvbox, Catppuccin, Tokyo Night, One Dark,
  Monokai and GitHub. Text colours, the roles' included, are at 4.5:1
  contrast on the theme's background and glyph colours at 3:1. `Span.role`
  styles text as a success, a warning, a failure, the main colour, muted
  text or a header, in the theme of the formatter it is printed on. `Theme.all` and
  `Theme.of_string` make a `--theme` option one line of Cmdliner.
- Motion. In a live display the frame of a table or a panel drawn above the
  rows (`Display.set_view`) moves in the theme's way (`Theme.motion`):
  digital rain in `Theme.matrix`, a rainbow sweep in `Theme.rainbow`, a
  shimmer in the colour schemes. Only colours and glyphs of one width
  change; printed with `Fmt`, on a pipe or without colour, a frame is still,
  and `Theme.still` stops every motion and the spinner.
- Live progress. `Display` shows the tree of the running tasks, one row
  each, with a spinner or a bar, a count or bytes and a rate, above a
  summary line (`~summary`) with the work's time and count, and redraws
  only those rows. A task that ends stays under its parent, marked, until
  its top task ends, and the top task's tree then goes to the scrollback
  whole, each task once under its parent; the summary is the last line
  printed. The region never scrolls the terminal: a tree too tall folds the
  tasks that ended into one line, then shows the latest running tasks. The
  region is a view (`Display.set_view`), a span the program puts together
  from `Display.tree`, `Display.summary` and any widget, such as a table
  above the rows or trees side by side with `~keep:true`; its last frame
  stays when the display closes. `Display.with_task` runs a
  function as a task, current in the fibers it forks, that succeeds when the
  function returns and fails when it raises. A task's output shows under it
  while it runs, and stays if the task fails: its `Logs` messages, its
  `Display.log` lines and its standard output on the display's terminal
  alike, printed whole (up to 1 MiB) under the line of a failure or a
  cancellation. `Display.within` makes a task current again, and output
  within `Display.root` goes above the rows, from inside a task too. In a
  pipe or a CI log, every line is printed once, in order with standard
  output under `2>&1`.
- `console-eio`. `Console_eio.run ~clock` runs a display on standard error
  with Eio: it gives it the `Logs` messages, those of other Eio domains
  included, and the program's output on its terminal, and gives the
  terminal back when the function returns or raises, when its fiber is
  cancelled, at exit and on SIGINT or SIGTERM. A `run` inside another, or
  in a fiber beside it, gets the same display. `~progress` chooses how
  progress shows, as `docker build --progress` does: `` `Auto ``, rows on a
  terminal and lines elsewhere; `` `Tty ``, rows on any output; `` `Plain ``,
  the lines a pipe gets; `` `Quiet ``, only what fails; `` `Json ``, one
  JSON object for each event. `Console.Progress.Event` types the events,
  and its [jsont](https://erratique.ch/software/jsont) codec,
  `Console.Progress.Event.jsont`, writes them and reads them back
  (`Event.of_string`), skipping the members, events and values that a later
  console adds. Times are rounded to the millisecond, and strings are valid
  UTF-8 without a C1 control. `Console.Progress.all`
  makes a `--progress=auto|tty|plain|quiet|json` option of it in one line
  of Cmdliner, as `Theme.all` makes a `--theme` option, and
  `Progress.to_string` names a mode. `Display.v ~progress` takes
  the same values without Eio, so a program with its own scheduler can be
  quiet or write JSON events too. `Console_eio.Prompt` asks a line, a yes or no, or a password, taking
  the rows off the screen while it asks, and tells the end of the input
  (`` `End_of_input ``) from its other errors.
- Colour policy. `console.tty` (`Console_tty.setup`) decides colour, its
  depth and UTF-8 for the standard outputs from the terminal, `NO_COLOR`,
  `TERM`, `COLORTERM` and the locale, or from a `--color` option, with Unix
  alone, and sets the theme of both (`~theme`). `Console_eio.run` decides
  only for the outputs the program left alone (`Console_tty.ensure`).
- Tests. `CONSOLE_FROZEN_CLOCK`, set and not empty, stops the display's
  clock at zero, so a cram test of a program's progress reads the same on
  every machine.
- Two small libraries in the `console` package: `console.text`
  (`Console_text`) makes untrusted text safe to print, and says where text
  read in pieces can be cut outside a character and an escape code
  (`Console_text.boundary`), and `console.input`
  (`Console_input`) is a line editor, with history and completion, that does
  no input or output itself.
- Known limitations, listed in the README: the tests read the screen
  through matrix 0.1.0's emulator, which they work around until matrix
  0.2.0; a pictograph with no emoji presentation, such as ❤ or ‼, counts
  one cell; and `Display.v` does not take the program's own output, which
  `Console_eio.run` does.
