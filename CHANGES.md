## unreleased

- `Console_input.v` no longer holds whatever a peer sends: a line stops at
  `?max_line` bytes (default 4095) and rings the bell past it, and the
  history keeps the last `?history_size` lines (default 500) (#1).

- `View.wrap`, and the table cells, panels, trees and progress rows that
  wrap, no longer break a line after a slash, so a path or a URL stays
  whole unless it is wider than the room (#2).

- `View.wrap ~words` says where a line may break, to keep a backquoted
  command whole or to break after a slash again (#2).

- `Console_input.crlf` maps each line feed to CR LF, for writing back to a
  peer whose terminal is in raw mode, so that each line starts in the first
  column (#3).

- `Progress.pp_duration` formats a duration as the rows and the summary
  show it, such as `6.4s` or `1m05s`, so that a program's own lines can
  match them (#4).

- `Console_eio.run` prints a `Logs.app` message as it was written, with no
  `[source]` header, as `Logs_fmt.reporter` does. Messages at the other
  levels keep their source's name (#5).

- On a pipe, `Console_eio.run` lays out what `Console_eio.print` prints, and
  the last frame, as wide as `$COLUMNS` asks; without it they are still
  printed whole (#7).

- `Task.advance` adds to a task's count of items or bytes and keeps its
  total, so a caller no longer keeps its own count to pass to
  `Task.set_items` (#8).

- `Color.gradient stops f` is the colour `f` of the way along `stops`, the
  one `View.gradient` and the bars grade with, for a program that draws
  its own colours, such as a pulse at a time (#9).

- `Console_eio.redraw ()` draws the display of the current run at once, so
  a program knows a frame is drawn without sleeping until the next timed
  redraw (#10).

- `Env.setup ~is_tty` and `Env.ensure ~is_tty` apply the colour, depth,
  UTF-8 and theme rules of `Console_tty.setup` to a formatter, so a program
  that cannot link Unix follows them too (#11).

## 0.9.0 (29/10/2026)

Initial release of `console` and `console-eio`.
