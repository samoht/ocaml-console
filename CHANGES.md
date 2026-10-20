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
- A theme names the glyphs of its bars (`Theme.bar`): heavy lines by
  default, full blocks on a light shade in `Theme.dos`, `#` on `-` in
  `Theme.ascii` and on any formatter that is not UTF-8.
- `Bar.to_span` puts a bar in a table cell, a tree or a panel, drawn in
  the theme and the UTF-8 of the formatter it is printed on.
- `Display.set_scene` draws what a printer prints, such as a table, above
  the rows of a live display, at their width and in the display's theme.
- A theme's frames move (`Theme.motion`) in a scene: digital rain in
  `Theme.matrix`, the rainbow sweeping round them in `Theme.rainbow`, a
  shimmer of the main colour in the themes of colour schemes. Only colours
  and glyphs change, never a width; printed with `Fmt`, on a pipe or
  without colour, a frame is still. `Theme.still` stops every motion, the
  spinner's too.
