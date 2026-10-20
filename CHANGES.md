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
