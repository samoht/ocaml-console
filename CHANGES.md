## 0.9.0

Initial release of `console` and `console-eio`.

- Views. A `View.t` is what console prints: text in one or more styles, with
  links (`View.link`, percent-encoding the bytes a URI may not hold),
  colours graded along it (`View.gradient`) and the colours of the theme
  (`View.role`), and every widget built from it. A view holds only text that
  prints, and `Style.styled` styles any `Fmt` printer. A view is what is
  drawn in a given room at a given time: `View.fit` gives a part its `Room`,
  the width and height it may take, the time of the frame and the
  formatter's theme and UTF-8; `View.pp` gives the margin and time 0, a
  display the terminal and the frame's time, `View.sized` narrows the room,
  `View.place` pads or cuts a view to a box, and `View.crop` takes a window
  of its cells. A view's size depends on its room alone. `View.hcat` puts
  views side by side, those that grow (`?grow`) sharing what the others
  leave in a room, and `View.vcat` one under the other (`?gap` blank lines
  apart). Widths are counted in terminal cells, by grapheme cluster with
  uuseg and uucp, wide characters and emoji included.
- Widgets. One constructor each, a view: `Table.view` (headers and cells are
  views, laid out at their column's width, columns that wrap or truncate,
  rules between rows with `~rules:true`, `?gap` spaces between cells, a
  column of a given `?width` that narrows as the others do, a frameless
  `Border.simple`), `Tree.view` (`?guide`, `?indent`), `Panel.view`
  (`?expand`), `Rule.view`, `Bar.horizontal`, `Bar.vertical` (with a `?peak`
  mark), `Bar.sparkline`, `Bar.chart` (a style for each bar) and
  `Spinner.view`. Widgets nest: a bar or a table in a cell, a table in a
  panel, trees side by side. A widget is no wider than its room, and is
  drawn in ASCII on a formatter that is not UTF-8. A part laid out for its
  room alone is laid out once for each room.
- Themes. A theme (`Theme.set`, kept on the formatter as its colours are)
  gives every view its colours, marks, frames, tree lines, bars and spinner
  frames. Eighteen are predefined: `Theme.default`, drawn in the
  terminal's own named colours (cyan, green, yellow and red) with faint
  muted text (`Theme.muted` is `None`), so it reads on the background the
  terminal's user chose, `ascii`, `dos`, `matrix`, `rainbow`, and the
  colour schemes of Dracula, Nord, Solarized, gruvbox, Catppuccin, Tokyo
  Night, One Dark, Monokai and GitHub. In a theme with colours of its own,
  text colours, the roles' included, are at 4.5:1 contrast on the theme's
  background and glyph colours at 3:1. The roles
  are those of a task's ending: `` `Succeeded ``, `` `Warning ``,
  `` `Failed ``, and the main colour, muted text and headers. `Theme.all`
  and `Theme.of_string` make a `--theme` option one line of Cmdliner.
- Motion. In a display that draws in colour, the frame of a table or a
  panel moves in the theme's way (`Theme.motion`): digital rain in
  `Theme.matrix`, a rainbow sweep in `Theme.rainbow`, a shimmer in the
  colour schemes, with or without tasks. A `Spinner.view` turns at the
  frame's time. Only colours and glyphs of one width change; printed once,
  on a pipe or without colour, a frame is still and a spinner shows its
  first frame, and `Theme.still` stops every motion.
- Tasks. `Task` is the work a program reports, with no drawing: a root
  (`Task.root`), and tasks under it (`Task.start parent label`,
  `Task.run parent label f`, which succeeds when `f` returns and fails,
  saying why, when it raises), each with a label, a count of items or bytes
  (`Task.set_items`, `Task.set_bytes`, whose rate, `Task.rate`, weighs the
  past by time, not by the number of reports), lines of output (`Task.log`, and its
  `Logs` messages and standard output under `Console_eio.run`, read back
  with `Task.output`, the latest 1 MiB, and `Task.dropped`) and an end
  (`Task.succeed`, `fail`, `cancel`). Ending the root says how the work
  ended, with a message, and ends every task under it, also one started
  afterwards. `Task.within` makes a task current, which decides only where
  output goes. Every change is a `Task.Event.t` (`Task.on_event`): an
  abstract value with a constructor for each event (`Event.succeeded`,
  `failed` and `cancelled` for the end of a task or of the work), an
  accessor for each member, and a
  [jsont](https://erratique.ch/software/jsont) codec that writes them and reads them back, skipping the members, events
  and values that a later console adds. Times are rounded to the
  millisecond and held within 10^12 seconds, and strings are valid UTF-8
  without a C1 control.
- Live progress. `Display` draws a view at the bottom of the terminal and
  redraws only the lines that change, in one synchronized update, never
  taller than the terminal less one row; `Display.print` writes a view once
  above it, at once. `~delay` holds the region back for its first seconds,
  so that quick work draws none. `Progress.show` connects a root to a display:
  `Progress.view` draws the tree of the running tasks (`Progress.tree`),
  one row each (`Progress.row`, drawn by a `Progress.Layout` of combinators
  with a spinner, the label, bold or in a style of its own, a bar, a count, a rate, the time left and the elapsed
  time, which give way in stages before the label is cut, and
  `Layout.choose`, a layout for each task), above a summary
  line (`Progress.summary`). A row is packed from the left: its label,
  its bar, up to 40 cells, then its count, rate and time, each in a column
  that all rows share, so the bars line up and nothing moves sideways. A
  task that ends stays under its parent, marked, until its top task ends,
  and the top task then goes to the scrollback as its commit draws it, its
  whole tree by default (`~commit`). A tree too tall folds the tasks that
  ended into one line, then drops its windows' oldest lines, then shows the
  latest running tasks; a table or a panel too tall goes whole. `~progress`
  chooses how progress shows, as `docker build --progress` does:
  `` `Auto ``, rows on a terminal and lines elsewhere; `` `Tty ``, rows on
  any output; `` `Plain ``, the lines a pipe gets, each naming its task by
  its path, such as `linux ▸ compile`, in order with standard output under
  `2>&1`; `` `Quiet ``, only what fails; `` `Json ``, one JSON object for
  each event. `Progress.all` makes a `--progress` option in one line of
  Cmdliner. Where tasks print as lines, `~plain` prints the program's own
  line for each event of a task in place of console's.
- `console-eio`. `Console_eio.run ~clock f` makes the root of the work,
  shows its tasks on standard error with Eio, as `Progress.show` does and
  with its options, passes `f` the root and ends the work as `Task.run`
  ends a task. It gives the current task the `Logs`
  messages, those of other Eio domains included, and the program's output
  on its terminal, and gives the terminal back when `f` returns or raises,
  when its fiber is cancelled, at exit and on SIGINT or SIGTERM. A `run`
  inside another, or in a fiber beside it, gets the task current where it
  is called. `Console_eio.print` prints a view above the rows,
  styled and as wide as the terminal, and `Console_eio.with_output` what
  console does not see. `Console_eio.Prompt` asks a line, a yes or no, or a
  password, taking the rows off the screen while it asks, and tells the end
  of the input (`` `End_of_input ``) from its other errors.
- Colour policy. `console.tty` (`Console_tty.setup`) decides colour, its
  depth and UTF-8 for the standard outputs from the terminal, `NO_COLOR`,
  `TERM`, `COLORTERM` and the locale, or from a `--color` option, with Unix
  alone, and sets the theme of both (`~theme`). `Console_eio.run` decides
  only for the outputs the program left alone (`Console_tty.ensure`).
- Tests. `CONSOLE_DETERMINISTIC_CLOCK`, set and not empty, stops the
  display's clock at zero, or makes a clock of `Console_eio.clock` virtual,
  as a mock clock is: a sleep wakes at the time it started plus its
  duration, sleeps at once overlap, and each still lasts its duration in
  real time. A cram test of a program's progress then reads the same on
  every machine, whatever the program computes between its sleeps.
  `CONSOLE_CAST=FILE`, which implies it, records what a run writes on the
  terminal to `FILE` in asciicast v2, at the times of the virtual clock, on a
  terminal of `$COLUMNS` by `$LINES` with no pty behind it, and the
  sleeps then take no real time: a recording is the same bytes on every
  run. `Console_tty.setup` starts the recording too, for a program that
  prints without a run.
- Two small libraries in the `console` package: `console.text`
  (`Console_text`) makes untrusted text safe to print, without the
  characters that Unicode makes default ignorable, which reorder or hide
  text, but with the flags of England, Scotland and Wales whole, and says
  where text read in pieces can be cut outside a character and an escape
  code (`Console_text.boundary`), and `console.input` (`Console_input`) is a
  line editor, with history and completion, that does no input or output
  itself.
- Known limitations, listed in the README: the tests read the screen through
  matrix 0.1.0's emulator, which they work around until matrix 0.2.0; a
  pictograph with no emoji presentation, such as ❤ or ‼, counts one cell;
  and `Progress.show` does not take the program's own output, which
  `Console_eio.run` does.
