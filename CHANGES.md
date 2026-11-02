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

## 0.9.0 (29/10/2026)

Initial release of `console` and `console-eio`.
