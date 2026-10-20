## 1.0.0

- Initial version.
- Eighteen themes (`Console.Theme`), from `Theme.default`, which reads on
  any background, to the colour schemes of Dracula, Nord, Solarized,
  gruvbox, Catppuccin, Tokyo Night, One Dark, Monokai and GitHub. A theme
  is kept on the formatter (`Theme.set`), as its colour depth is, and every
  widget is drawn in it; `Theme.all` and `Theme.of_string` give a
  `--theme` option in one line. Text colours are at 4.5:1 contrast on the
  theme's background and glyph colours at 3:1, and every widget falls back
  to ASCII on a formatter that is not UTF-8.
- Colours are written at the colour depth of their formatter
  (`Color.set_depth`), 16 colours until one is set: a program that turns
  colour on with Fmt alone sends codes that every terminal reads.
- `console.tty` (`Console_tty.setup`) decides colour, its depth and UTF-8
  for the standard outputs from the terminal, `NO_COLOR`, `TERM`,
  `COLORTERM` and the locale, with Unix alone: a program that prints a
  table needs no Eio. `Console_eio.run` calls `Console_tty.ensure`, which
  decides only for the outputs the program left alone.
- `Style.styled` styles any `Fmt` printer at its formatter's colour depth.
- `Span.gradient` grades text from one colour to the next, each grapheme
  cluster in the blend at its place, by default through the palette of
  the theme it is printed in.
- A theme names the glyphs of its bars (`Theme.bar`): heavy lines by
  default, full blocks on a light shade in `Theme.dos`, `#` on `-` in
  `Theme.ascii` and on any formatter that is not UTF-8.
- `Theme.dos` draws in the box-drawing characters of code page 437, as
  Norton Commander did: frames of double lines divided by single ones
  (`Border.dos`), and single-line tree guides. A border names the line
  between two columns and the line of a rule across the box
  (`Border.chars`), which `Border.dos` draws apart from its frame.
- A bar given no style spreads a palette of several colours along its
  length, each cell in the colour of its place: `Theme.rainbow`'s bars,
  its progress rows' included, are rainbows.
- `Bar.to_span` puts a bar in a table cell, a tree or a panel, drawn in
  the theme and the UTF-8 of the formatter it is printed on.
- `Table.v ~rules:true` separates the rows of a table with rules.
- `Console.hcat` prints blocks side by side, such as a table beside a
  tree, each at its share of the margin and in its colours; a scene can
  be one.
- `Display.set_scene` draws what a printer prints, such as a table, above
  the rows of a live display, at their width and in the display's theme.
- A theme's frames move (`Theme.motion`) in a scene: digital rain running
  along an intact frame in `Theme.matrix`, a rainbow graded round them and
  sweeping in `Theme.rainbow`, a shimmer of the main colour in the themes
  of colour schemes. Only colours and the glyphs of straight lines change,
  never a width; printed with `Fmt`, on a pipe or without colour, a frame
  is still, a sweep keeping its colours. `Theme.still` stops every motion,
  the spinner's too.
- A display keeps two kinds of lines apart: `Logs` messages, the program's
  diagnostics, are always kept, printed after the name of the task they
  were logged in; the output of a task, given to `Display.log`, shows in
  its window while it runs, goes when it succeeds, is printed whole under
  its line when it fails (up to 1 MiB, the oldest lines dropped and
  counted) and stays in its window when it is cancelled, Ctrl-C included.
  On a pipe a task prints a line when it starts, every line at once after
  its name, and repeats its window under a failure. `Display.with_task`
  makes a task current for a function and the fibers it forks, and ends it
  when the function returns or raises; `Display.iter` counts a list.
  `Display.report` and `Display.scope` are for drivers such as
  `Console_eio.run`; `Display.message` is gone, and `Display.log` has no
  level.
- `Display.set_result` takes a `(string, string) result`, the row layout is
  `Display.Layout`, and a closed display ignores what it is told rather
  than raising, so a fiber that outlives `Console_eio.run` is harmless.
- `Theme.mark` has a `` `Debug `` mark for debug messages.
- `Console_eio.run ~clock` takes a monotonic clock
  (`Eio.Stdenv.mono_clock env`), which times the display and paces its
  redraws, so a change of the system's time moves no timer.
