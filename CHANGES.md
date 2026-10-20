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
